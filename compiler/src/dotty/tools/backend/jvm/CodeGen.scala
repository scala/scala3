package dotty.tools.backend.jvm


import dotty.tools.dotc.ast.Trees.{PackageDef, ValDef}
import dotty.tools.dotc.ast.tpd

import scala.collection.mutable
import dotty.tools.dotc.{CompilationUnit, interfaces, report, util}
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
import dotty.tools.dotc.ast.Positioned
import dotty.tools.dotc.util.NoSourcePosition
import SymbolUtils.given
import dotty.tools.backend.ScalaPrimitives
import dotty.tools.dotc.interfaces.CompilerCallback
import opt.{OptimizerUtils, CallGraph}

class CodeGen(val primitives: ScalaPrimitives,
              val callGraph: Option[CallGraph], val bTypeLoader: BTypeLoader, val knownBTypes: KnownBTypes,
              val generatedClassHandler: GeneratedClassHandler) {
  private class Impl extends BCodeIdiomatic(callGraph), BCodeHelpers(bTypeLoader), BCodeBodyBuilder(primitives, knownBTypes), BCodeSyncAndTry
  private val impl = new Impl()

  private lazy val mirrorCodeGen = impl.JMirrorBuilder()

  /**
   * Generate ASM ClassNodes for classes found in the context's compilation unit. The resulting classes are
   * passed to the `generatedClassHandler`.
   */
  def genUnit()(using ctx: Context): Unit = {
    val generatedClasses = mutable.ListBuffer.empty[GeneratedClass]
    val generatedTasty = mutable.ListBuffer.empty[GeneratedTasty]

    def genClassDef(cd: TypeDef): Unit =
      try
        val sym = cd.symbol
        val sourceFile = ctx.compilationUnit.source.file
        val mainClassNode = genClass(cd)
        val mirrorClassNode =
          if !sym.isTopLevelModuleClass then null
          else if sym.companionClass == NoSymbol then mirrorCodeGen.genMirrorClass(sym)
          else
            report.log(s"No mirror class for module with linked class: ${sym.fullName}", NoSourcePosition)
            null

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
    generatedClassHandler.process(
      GeneratedCompilationUnit(ctx.compilationUnit.source.file, generatedClasses.toList, generatedTasty.toList)
    )
  }

  // Creates a callback that will be evaluated in PostProcessor after creating a file
  private def onFileCreated(cls: ClassNode, claszSymbol: Symbol, sourceFile: util.SourceFile)(using Context): AbstractFile => Unit = {
    val isLocal = atPhase(sbtExtractDependenciesPhase) {
      claszSymbol.isLocal
    }
    clsFile => {
      val className = cls.name.replace('/', '.')
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

  private def genClass(cd: TypeDef)(using Context): ClassNode = {
    val b = new impl.SyncAndTryBuilder
    b.genPlainClass(cd)
  }

}
