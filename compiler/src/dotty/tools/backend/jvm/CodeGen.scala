package dotty.tools.backend.jvm

import scala.language.unsafeNulls

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.Trees.{PackageDef, ValDef}
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Phases.Phase

import scala.collection.mutable
import scala.jdk.CollectionConverters.*

import dotty.tools.dotc.interfaces
import dotty.tools.dotc.report

import java.util.Optional
import dotty.tools.dotc.sbt.ExtractDependencies
import dotty.tools.dotc.core.*
import Contexts.*
import Phases.*
import Symbols.*
import StdNames.nme

import java.io.DataOutputStream
import java.nio.channels.ClosedByInterruptException

import dotty.tools.tasty.{ TastyBuffer, TastyHeaderUnpickler }

import scala.tools.asm
import scala.tools.asm.tree.*
import tpd.*
import dotty.tools.io.AbstractFile
import dotty.tools.dotc.util
import dotty.tools.dotc.util.NoSourcePosition


class CodeGen(val int: DottyBackendInterface, val primitives: DottyPrimitives)( val bTypes: BTypesFromSymbols[int.type]) { self =>
  import DottyBackendInterface.symExtensions
  import bTypes.*
  import int.given

  private lazy val mirrorCodeGen = Impl.JMirrorBuilder()

  def genUnit(unit: CompilationUnit): GeneratedDefs = {
    val generatedClasses = mutable.ListBuffer.empty[GeneratedClass]
    val generatedTasty = mutable.ListBuffer.empty[GeneratedTasty]

    def genClassDef(cd: TypeDef): Unit =
      try
        val sym = cd.symbol
        val sourceFile = unit.source.file

        def registerGeneratedClass(classNode: ClassNode, isArtifact: Boolean): Unit =
          generatedClasses += GeneratedClass(classNode, sourceFile, isArtifact, onFileCreated(classNode, sym, unit.source))

        val plainC = genClass(cd, unit)
        registerGeneratedClass(plainC, isArtifact = false)

        val attrNode =
          if !sym.isTopLevelModuleClass then plainC
          else if sym.companionClass == NoSymbol then
            val mirrorC = genMirrorClass(sym, unit)
            registerGeneratedClass(mirrorC, isArtifact = true)
            mirrorC
          else
            report.log(s"No mirror class for module with linked class: ${sym.fullName}", NoSourcePosition)
            plainC

        if sym.isClass then
          genTastyAndSetAttributes(sym, attrNode)
      catch
        case ex: Throwable =>
          ex.printStackTrace()
          report.error(s"Error while emitting ${unit.source}\n${ex.getMessage}", NoSourcePosition)


    def genTastyAndSetAttributes(claszSymbol: Symbol, store: ClassNode): Unit =
      import Impl.createJAttribute
      for (binary <- unit.pickled.get(claszSymbol.asClass)) {
        generatedTasty += GeneratedTasty(store, binary)
        val tasty =
          val uuid = new TastyHeaderUnpickler(binary()).readHeader()
          val lo = uuid.getMostSignificantBits
          val hi = uuid.getLeastSignificantBits

          // TASTY attribute is created but only the UUID bytes are stored in it.
          // A TASTY attribute has length 16 if and only if the .tasty file exists.
          val buffer = new TastyBuffer(16)
          buffer.writeUncompressedLong(lo)
          buffer.writeUncompressedLong(hi)
          buffer.bytes

        val dataAttr = createJAttribute(nme.TASTYATTR.mangledString, tasty, 0, tasty.length)
        store.visitAttribute(dataAttr)
      }

    def genClassDefs(tree: Tree): Unit =
      tree match {
        case EmptyTree => ()
        case PackageDef(_, stats) => stats foreach genClassDefs
        case ValDef(_, _, _) => () // module val not emitted
        case td: TypeDef =>  genClassDef(td)
      }

    genClassDefs(unit.tpdTree)
    GeneratedDefs(generatedClasses.toList, generatedTasty.toList)
  }

  // Creates a callback that will be evaluated in PostProcessor after creating a file
  private def onFileCreated(cls: ClassNode, claszSymbol: Symbol, sourceFile: util.SourceFile): AbstractFile => Unit = clsFile => {
    val (fullClassName, isLocal) = atPhase(sbtExtractDependenciesPhase) {
      (ExtractDependencies.classNameAsString(claszSymbol), claszSymbol.isLocal)
    }

    val className = cls.name.replace('/', '.')
    if (ctx.compilerCallback != null)
      ctx.compilerCallback.onClassGenerated(sourceFile, convertAbstractFile(clsFile), className)

    ctx.withIncCallback: cb =>
      if (isLocal) cb.generatedLocalClass(sourceFile, clsFile.jpath)
      else cb.generatedNonLocalClass(sourceFile, clsFile.jpath, className, fullClassName)
  }

  /** Convert a `dotty.tools.io.AbstractFile` into a
   *  `dotty.tools.dotc.interfaces.AbstractFile`.
   */
  private def convertAbstractFile(absfile: dotty.tools.io.AbstractFile): interfaces.AbstractFile =
    new interfaces.AbstractFile {
      override def name = absfile.name
      override def path = absfile.path
      override def jfile = Optional.ofNullable(absfile.file)
    }

  private def genClass(cd: TypeDef, unit: CompilationUnit): ClassNode = {
    val b = new Impl.PlainClassBuilder(unit)
    b.genPlainClass(cd)
    val cls = b.cnode
    checkForCaseConflict(cls.name, cd.symbol)
    cls
  }

  private def genMirrorClass(classSym: Symbol, unit: CompilationUnit): ClassNode = {
    val cls = mirrorCodeGen.genMirrorClass(classSym, unit)
    checkForCaseConflict(cls.name, classSym)
    cls
  }

  private val lowerCaseNames = mutable.HashMap.empty[String, Symbol]
  private def checkForCaseConflict(javaClassName: String, classSymbol: Symbol) = {
    val lowerCaseName = javaClassName.toLowerCase
    lowerCaseNames.get(lowerCaseName) match {
      case None =>
        lowerCaseNames.put(lowerCaseName, classSymbol)
      case Some(dupClassSym) =>
        // Order is not deterministic so we enforce lexicographic order between the duplicates for error-reporting
        val (cl1, cl2) =
          if (classSymbol.effectiveName.toString < dupClassSym.effectiveName.toString) (classSymbol, dupClassSym)
          else (dupClassSym, classSymbol)
        val same = classSymbol.effectiveName.toString == dupClassSym.effectiveName.toString
        atPhase(typerPhase) {
          if same then
             // FIXME: This should really be an error, but then FromTasty tests fail
            report.warning(s"${cl1.show} and ${cl2.showLocated} produce classes that overwrite one another", cl1.sourcePos)
          else
            report.warning(s"${cl1.show} differs only in case from ${cl2.showLocated}. " +
              "Such classes will overwrite one another on case-insensitive filesystems.", cl1.sourcePos)
        }
    }
  }

  sealed transparent trait ImplEarlyInit{
    val int: self.int.type = self.int
    val bTypes: self.bTypes.type = self.bTypes
    protected val primitives: DottyPrimitives = self.primitives
  }
  object Impl extends ImplEarlyInit with BCodeSyncAndTry {
    class PlainClassBuilder(unit: CompilationUnit) extends SyncAndTryBuilder(unit)
  }
}
