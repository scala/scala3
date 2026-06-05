package dotty.tools.backend.jvm


import dotty.tools.dotc.ast.Trees.{PackageDef, ValDef}
import dotty.tools.dotc.ast.tpd

import scala.collection.mutable
import dotty.tools.dotc.sbt.ExtractDependencies
import dotty.tools.dotc.core.*
import Contexts.*
import Phases.*
import Symbols.*
import StdNames.nme
import dotty.tools.tasty.{TastyBuffer, TastyHeaderUnpickler}
import dotty.tools.dotc.core.tasty.TastyUnpickler

import scala.tools.asm.tree.*
import tpd.*
import dotty.tools.io.AbstractFile
import dotty.tools.dotc.interfaces.CompilerCallback
import dotty.tools.dotc.report
import dotty.tools.dotc.util.SourceFile

class CodeGen(impl: BCodeSyncAndTry) {
  private val builder = new impl.SyncAndTryBuilder()
  private val mirrorBuilder = new impl.JMirrorBuilder()

  /**
   * Generate ASM ClassNodes for classes found in the context's compilation unit.
   */
  def genUnit()(using ctx: Context): GeneratedCompilationUnit = {
    val generatedClasses = mutable.ListBuffer.empty[GeneratedClass]
    val generatedTasty = mutable.ListBuffer.empty[GeneratedTasty]

    def genClassDef(cd: TypeDef): Unit =
      try
        val sym = cd.symbol
        val mainClassNode = builder.genPlainClass(cd, topLevel = true)
        val mirrorClassNode = mirrorBuilder.genMirrorClassIfNeeded(sym)

        if sym.isClass then
          val tastyAttrNode = if (mirrorClassNode ne null) mirrorClassNode else mainClassNode
          genTastyAndSetAttributes(sym, tastyAttrNode)

        def registerGeneratedClass(classNode: ClassNode | Null, isArtifact: Boolean): Unit =
          if classNode ne null then
            generatedClasses += GeneratedClass(classNode,
              sourceClassName = sym.javaClassName,
              position = sym.srcPos.sourcePos,
              isArtifact = isArtifact,
              onFileCreated = onFileCreated(classNode, sym, ctx.compilationUnit.source)
            )

        registerGeneratedClass(mainClassNode, isArtifact = false)
        registerGeneratedClass(mirrorClassNode, isArtifact = true)
      catch
        case ex: TypeError =>
          report.error(s"Error while emitting ${ctx.compilationUnit.source}\n${ex.getMessage}", cd.sourcePos)

    def genTastyAndSetAttributes(claszSymbol: Symbol, store: ClassNode): Unit =
      for (binary <- ctx.compilationUnit.pickled.get(claszSymbol.asClass)) {
        generatedTasty += GeneratedTasty(store, binary)
        val tasty =
          val uuid = new TastyHeaderUnpickler(TastyUnpickler.scala3CompilerConfig, binary()).readHeader()
          val lo = uuid.getMostSignificantBits
          val hi = uuid.getLeastSignificantBits

          // TASTY attribute is created but only the UUID bytes are stored in it.
          // A TASTY attribute has length 16 if and only if the .tasty file exists.
          val buffer = new TastyBuffer(16)
          buffer.writeUncompressedLong(lo)
          buffer.writeUncompressedLong(hi)
          buffer.bytes

        val dataAttr = impl.createJAttribute(nme.TASTYATTR.mangledString, tasty, 0, tasty.length)
        store.visitAttribute(dataAttr)
      }

    def genClassDefs(tree: Tree): Unit =
      tree match {
        case EmptyTree => ()
        case PackageDef(_, stats) => stats.foreach(genClassDefs)
        case ValDef(_, _, _) => () // module val not emitted
        case td: TypeDef => genClassDef(td)
      }

    genClassDefs(ctx.compilationUnit.tpdTree)
    GeneratedCompilationUnit(ctx.compilationUnit.source.file, generatedClasses.toList, generatedTasty.toList)
  }

  // Creates a callback that will be evaluated in PostProcessor after creating a file
  private def onFileCreated(cls: ClassNode, claszSymbol: Symbol, sourceFile: SourceFile)(using Context): AbstractFile => Unit = {
    val isLocal = atPhase(sbtExtractDependenciesPhase) {
      claszSymbol.isLocal
    }
    val className = cls.name.replace('/', '.')
    clsFile => {
      ctx.compilerCallback match
        case cb: CompilerCallback => cb.onClassGenerated(sourceFile, clsFile, className)
        case null => ()

      ctx.withIncCallback: cb =>
        if isLocal then
          cb.generatedLocalClass(sourceFile, clsFile.jpath)
        else if !cb.enabled() then
          // callback is not enabled, so nonLocalClasses were not reported in ExtractAPI
          val fullClassName = atPhase(sbtExtractDependenciesPhase) {
            ExtractDependencies.classNameAsString(claszSymbol)
          }
          cb.generatedNonLocalClass(sourceFile, clsFile.jpath, className, fullClassName)
    }
  }
}
