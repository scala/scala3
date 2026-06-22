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
import dotty.tools.dotc.core.Decorators.em
import dotty.tools.tasty.{TastyBuffer, TastyHeaderUnpickler}
import dotty.tools.dotc.core.tasty.TastyUnpickler

import scala.tools.asm.tree.*
import tpd.*
import dotty.tools.io.AbstractFile
import dotty.tools.dotc.interfaces.CompilerCallback
import dotty.tools.dotc.report
import dotty.tools.dotc.sbt.interfaces.IncrementalCallback
import dotty.tools.dotc.util.{SourceFile, SourcePosition}

class CodeGen(impl: BCodeSyncAndTry) {
  private val caseInsensitively = new java.util.HashMap[String, (String, SourcePosition)]

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
        // This builder cannot be shared as it includes per-class mutable state that is not reset
        val mainClassNode = new impl.SyncAndTryBuilder().genPlainClass(cd, topLevel = true)
        val mirrorClassNode = mirrorBuilder.genMirrorClassIfNeeded(sym)

        if sym.isClass then
          val tastyAttrNode = if (mirrorClassNode ne null) mirrorClassNode else mainClassNode
          genTastyAndSetAttributes(sym, tastyAttrNode)

        def registerGeneratedClass(classNode: ClassNode | Null): Unit =
          if classNode ne null then
            val className = classNode.name.replace('/', '.')
            val (fullClassName, isLocal) = atPhase(sbtExtractDependenciesPhase) {
              (ExtractDependencies.classNameAsString(sym), sym.isLocal)
            }
            generatedClasses += GeneratedClass(
              classNode = classNode,
              position = sym.srcPos.sourcePos,
              onFileCreated = onFileCreated(className, fullClassName, isLocal, ctx.compilationUnit.source,
                                            ctx.compilerCallback, ctx.incCallback)
            )

        registerGeneratedClass(mainClassNode)
        registerGeneratedClass(mirrorClassNode)
      catch
        case ex: TypeError =>
          report.error(s"Error while emitting ${ctx.compilationUnit.source}\n${ex.getMessage}", cd.sourcePos)

    def genTastyAndSetAttributes(claszSymbol: Symbol, store: ClassNode): Unit =
      for (binary <- ctx.compilationUnit.pickled.get(claszSymbol.asClass)) {
        generatedTasty += GeneratedTasty(store.name, binary)
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
    // Order is not deterministic so we enforce lexicographic order for error-reporting
    generatedClasses.sortBy(_.classNode.name).foreach(c => warnCaseInsensitiveOverwrite(c.classNode.name, c.position))
    GeneratedCompilationUnit(ctx.compilationUnit.source.file.canonicalPath, generatedClasses.toList, generatedTasty.toList)
  }

  private def warnCaseInsensitiveOverwrite(name: String, clsPos: SourcePosition)(using Context): Unit =
    caseInsensitively.putIfAbsent(name.toLowerCase, (name, clsPos)) match {
      case null => ()
      case (dupName, dupPos) =>
        val locationAddendum =
          if clsPos.source.path == dupPos.source.path then ""
          else s" (defined in ${dupPos.source.file.name})"
        def nicify(name: String): String = name.replace('/', '.')
        if name == dupName then
          report.error(
            em"${nicify(name)} and ${nicify(dupName)} produce classes that overwrite one another", clsPos)
        else
          report.warning(
            em"""Generated class ${nicify(name)} differs only in case from ${nicify(dupName)}$locationAddendum.
                |  Such classes will overwrite one another on case-insensitive filesystems.""", clsPos)
    }

  // Creates a callback that will be evaluated in PostProcessor after creating a file
  private def onFileCreated(className: String, fullClassName: String, isLocal: Boolean, sourceFile: SourceFile,
                            compilerCallback: CompilerCallback | Null, incrementalCallback: IncrementalCallback | Null)
                           (clsFile: AbstractFile): Unit = {
    compilerCallback match
      case null => ()
      case cb => cb.onClassGenerated(sourceFile, clsFile, className)

    incrementalCallback match
      case null => ()
      case cb if isLocal => cb.generatedLocalClass(sourceFile, clsFile.jpath)
      case cb if !cb.enabled() =>
        // callback is not enabled, so nonLocalClasses were not reported in ExtractAPI
        cb.generatedNonLocalClass(sourceFile, clsFile.jpath, className, fullClassName)
      case cb => ()
  }
}
