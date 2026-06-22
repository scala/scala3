package dotty.tools.backend.jvm

import dotty.tools.backend.jvm.BTypes.InternalName

import dotty.tools.dotc.util.SourcePosition
import dotty.tools.io.AbstractFile
import dotty.tools.io.FileWriters
import dotty.tools.dotc.core.Contexts.Context

import scala.tools.asm.ClassWriter
import scala.tools.asm.tree.ClassNode
import dotty.tools.backend.jvm.opt.*

import scala.annotation.constructorOnly
import scala.collection.mutable
import scala.tools.asm

/**
 * Implements late stages of the backend, i.e.,
 * optimizations, post-processing and classfile serialization and writing.
 *
 * This base class doesn't do optimizations, use the subclass for that if they're enabled.
 */
class PostProcessor(classBTypeCache: ClassBType.Cache, bTypes: KnownBTypes)(using @constructorOnly initctx: Context) {

  private val classfileWriter: FileWriters.ClassfileWriter =
    FileWriters.ClassfileWriter(initctx.settings.outputDir.value, initctx.settings.XmainClass.valueSetByUser, initctx.settings.XjarCompressionLevel.value, initctx.settings.Xdumpclasses.value)

  final def sendToDisk(clazz: GeneratedClass): Unit = {
    val classNode = clazz.classNode
    runLocalOptimizations(classNode)
    setInnerClasses(classNode)
    val bytes = serializeClass(classNode)
    TraceUtils.traceSerializedClassIfRequested(classNode.name, bytes)
    val clsFile = classfileWriter.writeClass(classNode.name, bytes)
    clazz.onFileCreated(clsFile)
  }

  final def sendToDisk(tasty: GeneratedTasty): Unit = {
    val GeneratedTasty(internalName, tastyGenerator) = tasty
    classfileWriter.writeTasty(internalName, tastyGenerator())
  }

  def runGlobalOptimizations(generatedUnits: Iterable[GeneratedCompilationUnit], issueSink: OptimizerIssue => Unit): Unit =
    () // no optimizations by default

  protected def runLocalOptimizations(classNode: ClassNode): Unit =
    () // no optimizations by default

  final def close(): Unit =
    classfileWriter.close()

  private def setInnerClasses(classNode: ClassNode): Unit = {
    classNode.innerClasses.nn.clear()
    val (declared, referred) = collectNestedClasses(classNode)
    addInnerClasses(classNode, declared, referred)
  }

  /**
   * Visit the class node and collect all referenced nested classes.
   */
  private def collectNestedClasses(classNode: ClassNode): (Iterable[ClassBType], Iterable[ClassBType]) = {
    val c = new NestedClassesCollector[ClassBType](nestedOnly = true) {
      def declaredNestedClasses(internalName: InternalName): List[ClassBType] =
        classBTypeCache.previouslyConstructedClassBType(internalName).get.info.nestedClasses

      def getClassIfNested(internalName: InternalName): Option[ClassBType] = {
        val c = classBTypeCache.previouslyConstructedClassBType(internalName).get
        Option.when(c.isNestedClass)(c)
      }

      def raiseError(msg: String, sig: String, e: Option[Throwable]): Unit = {
        // don't crash on invalid generic signatures
      }
    }
    c.visit(classNode)
    (c.declaredInnerClasses, c.referredInnerClasses)
  }

  private def serializeClass(classNode: ClassNode): Array[Byte] = {
    val cw = new ClassWriterWithBTypeLub(asm.ClassWriter.COMPUTE_MAXS | asm.ClassWriter.COMPUTE_FRAMES)
    classNode.accept(cw)
    cw.toByteArray.nn
  }

  /*
   * Populates the InnerClasses JVM attribute with `refedInnerClasses`. See also the doc on inner
   * classes in BTypes.scala.
   *
   * `refedInnerClasses` may contain duplicates, need not contain the enclosing inner classes of
   * each inner class it lists (those are looked up and included).
   *
   * This method serializes in the InnerClasses JVM attribute in an appropriate order,
   * not necessarily that given by `refedInnerClasses`.
   *
   * can-multi-thread
   */
  private def addInnerClasses(jclass: asm.ClassVisitor, declaredInnerClasses: Iterable[ClassBType], refedInnerClasses: Iterable[ClassBType]): Unit = {
    // sorting ensures nested classes are listed after their enclosing class thus satisfying the Eclipse Java compiler
    val allNestedClasses = new mutable.TreeSet[ClassBType]()(using Ordering.by(_.internalName))
    allNestedClasses ++= declaredInnerClasses
    refedInnerClasses.foreach(allNestedClasses ++= _.enclosingNestedClassesChain)
    for nestedClass <- allNestedClasses
      do {
        // Extract the innerClassEntry - we know it exists, enclosingNestedClassesChain only returns nested classes.
        val Some(e) = nestedClass.innerClassAttributeEntry: @unchecked
        jclass.visitInnerClass(e.name, e.outerName, e.innerName, e.flags)
      }
  }


  // -----------------------------------------------------------------------------------------
  // finding the least upper bound in agreement with the bytecode verifier (given two internal names handed by ASM)
  // Background:
  //  http://gallium.inria.fr/~xleroy/publi/bytecode-verification-JAR.pdf
  //  http://comments.gmane.org/gmane.comp.java.vm.languages/2293
  //  https://github.com/scala/bug/issues/3872
  // -----------------------------------------------------------------------------------------

  /*  An `asm.ClassWriter` that uses `jvmWiseLUB()`
   *  The internal name of the least common ancestor of the types given by inameA and inameB.
   *  It's what ASM needs to know in order to compute stack map frames, http://asm.ow2.org/doc/developer-guide.html#controlflow
   */
  private final class ClassWriterWithBTypeLub(flags: Int) extends ClassWriter(flags) {
    /**
     * This method is used by asm when computing stack map frames.
     */
    override def getCommonSuperClass(inameA: String, inameB: String): String = {
      // All types that appear in a class node need to have their ClassBType cached,
      // i.e., have been loaded either from symbols or from class files.
      val a = classBTypeCache.previouslyConstructedClassBType(inameA).get
      val b = classBTypeCache.previouslyConstructedClassBType(inameB).get
      val lub = a.jvmWiseLUB(b, bTypes.ObjectRef)
      val lubName = lub.internalName
      assert(lubName != "scala/Any")
      lubName // ASM caches the answer during the lifetime of a ClassWriter. We outlive that. Not sure whether caching on our side would improve things.
    }
  }
}

final class PostProcessorWithOptimizations(classBTypeCache: ClassBType.Cache, byteCodeRepository: BCodeRepository, bTypesFromClassfile: BTypesFromClassfile,
                                           callGraph: OptimizerCallGraph, indyTracker: IndyLambdaImplTracker,
                                           bTypes: OptimizerKnownBTypes)(using @constructorOnly initctx: Context) extends PostProcessor(classBTypeCache, bTypes) {
  private val optSettings         = new OptimizerSettings()
  private val closureOptimizer    = new ClosureOptimizer(indyTracker, byteCodeRepository, callGraph, bTypes, bTypesFromClassfile, optSettings)
  private val heuristics          = new InlinerHeuristics(byteCodeRepository, callGraph, bTypes, optSettings)
  private val inliner             = new GlobalOptimizer(indyTracker, callGraph, classBTypeCache, bTypesFromClassfile, byteCodeRepository, heuristics, closureOptimizer, optSettings)
  private val localOpt            = new LocalOptimizer(indyTracker, callGraph, inliner, bTypes, bTypesFromClassfile, optSettings)

  override def runGlobalOptimizations(generatedUnits: Iterable[GeneratedCompilationUnit], issueSink: OptimizerIssue => Unit): Unit = {
    inliner.run(generatedUnits.flatMap(u => u.classes.map(c => (c.classNode, u.sourcePath))), issueSink)
  }

  protected override def runLocalOptimizations(classNode: ClassNode): Unit =
    localOpt.run(classNode)
}

case class GeneratedClass(
  classNode: ClassNode,
  position: SourcePosition,
  onFileCreated: AbstractFile => Unit)
case class GeneratedTasty(internalName: String, tastyGen: () => Array[Byte])
case class GeneratedCompilationUnit(sourcePath: String, classes: List[GeneratedClass], tasty: List[GeneratedTasty])

