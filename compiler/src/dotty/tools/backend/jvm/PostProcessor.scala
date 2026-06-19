package dotty.tools.backend.jvm

import java.util.concurrent.ConcurrentHashMap
import dotty.tools.dotc.util.SourcePosition
import dotty.tools.io.AbstractFile
import dotty.tools.io.FileWriters
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Decorators.em

import scala.tools.asm.{ClassWriter, Handle}
import scala.tools.asm.tree.{ClassNode, InvokeDynamicInsnNode}
import dotty.tools.backend.jvm.opt.*
import dotty.tools.dotc.report
import dotty.tools.io.PlainFile.toPlainFile

import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters.*
import scala.collection.mutable
import scala.tools.asm
import scala.util.chaining.scalaUtilChainingOps

/**
 * Implements late stages of the backend, i.e.,
 * optimizations, post-processing and classfile serialization and writing.
 *
 * This base class doesn't do optimizations, use the subclass for that if they're enabled.
 */
class PostProcessor(bTypeLoader: BTypeLoader, bTypes: KnownBTypes)(using Context) {

  private val classfileWriter: FileWriters.ClassfileWriter = {
    val dumpClassesPath =
      ctx.settings.Xdumpclasses.valueSetByUser
        .map(p => Paths.get(p))
        .filter(path => Files.exists(path).tap(ok => if !ok then report.error(em"Output dir does not exist: ${path.toString}")))
        .map(_.toPlainFile)

    FileWriters.ClassfileWriter(ctx.settings.outputDir.value, ctx.settings.XmainClass.valueSetByUser, ctx.settings.XjarCompressionLevel.value, dumpClassesPath)
  }

  private type ClassnamePosition = (String, SourcePosition)
  private val caseInsensitively = new ConcurrentHashMap[String, ClassnamePosition]

  final def sendToDisk(clazz: GeneratedClass): Unit = {
    val classNode = clazz.classNode
    val internalName = classNode.name.nn
    val bytes =
      try
        if !clazz.isArtifact then
          runLocalOptimizations(classNode)
          addLambdaDeserialize(classNode)
        warnCaseInsensitiveOverwrite(clazz)
        setInnerClasses(classNode)
        serializeClass(classNode)
      catch
        case ex: Exception =>
          if ctx.debug then ex.printStackTrace()
          report.error(em"Error while emitting $internalName\n${ex.getMessage}")
          null

    if bytes != null then
      TraceUtils.traceSerializedClassIfRequested(internalName, bytes)
      val clsFile = classfileWriter.writeClass(internalName, bytes)
      clazz.onFileCreated(clsFile)
  }

  final def sendToDisk(tasty: GeneratedTasty): Unit = {
    val GeneratedTasty(classNode, tastyGenerator) = tasty
    val internalName = classNode.name.nn
    classfileWriter.writeTasty(classNode.name.nn, tastyGenerator())
  }

  def runGlobalOptimizations(generatedUnits: Iterable[GeneratedCompilationUnit]): Unit =
    () // no optimizations by default

  protected def runLocalOptimizations(classNode: ClassNode): Unit =
    () // no optimizations by default

  final def close(): Unit =
    classfileWriter.close()

  private def warnCaseInsensitiveOverwrite(clazz: GeneratedClass): Unit = {
    val name = clazz.classNode.name
    val lowerCaseJavaName = name.toLowerCase
    val clsPos = clazz.position
    caseInsensitively.putIfAbsent(lowerCaseJavaName, (name, clsPos)) match {
      case null => ()
      case (dupName, dupPos) =>
        // Order is not deterministic so we enforce lexicographic order between the duplicates for error-reporting
        val ((pos1, pos2), (name1, name2)) =
          if (name < dupName) ((clsPos, dupPos), (name, dupName))
          else ((dupPos, clsPos), (dupName, name))
        val locationAddendum =
          if pos1.source.path == pos2.source.path then ""
          else s" (defined in ${pos2.source.file.name})"
        def nicify(name: String): String = name.replace('/', '.')
        if name1 == name2 then
          report.error(
            em"${nicify(name1)} and ${nicify(name2)} produce classes that overwrite one another", pos1)
        else
          report.warning(
            em"""Generated class ${nicify(name1)} differs only in case from ${nicify(name2)}$locationAddendum.
                |  Such classes will overwrite one another on case-insensitive filesystems.""", pos1)
    }
  }

  private def setInnerClasses(classNode: ClassNode): Unit = {
    classNode.innerClasses.nn.clear()
    val (declared, referred) = bTypeLoader.collectNestedClasses(classNode)
    addInnerClasses(classNode, declared, referred)
  }

  private def serializeClass(classNode: ClassNode): Array[Byte] = {
    val cw = new ClassWriterWithBTypeLub(asm.ClassWriter.COMPUTE_MAXS | asm.ClassWriter.COMPUTE_FRAMES)
    classNode.accept(cw)
    cw.toByteArray.nn
  }

  private def collectSerializableLambdas(classNode: ClassNode): Array[Handle] = {
    val indyLambdaBodyMethods = new mutable.ArrayBuffer[Handle]
    for (m <- classNode.methods.asScala) {
      val iter = m.instructions.iterator
      while (iter.hasNext) {
        val insn = iter.next()
        insn match {
          case indy: InvokeDynamicInsnNode
            if indy.bsm == bTypes.jliLambdaMetaFactoryAltMetafactoryHandle =>
            import java.lang.invoke.LambdaMetafactory.FLAG_SERIALIZABLE
            val metafactoryFlags = indy.bsmArgs(3).asInstanceOf[Integer].toInt
            val isSerializable = (metafactoryFlags & FLAG_SERIALIZABLE) != 0
            if isSerializable then
              val implMethod = indy.bsmArgs(1).asInstanceOf[Handle]
              indyLambdaBodyMethods += implMethod
          case _ =>
        }
      }
    }
    indyLambdaBodyMethods.toArray
  }

  /*
  * Add:
  *
  * private static Object $deserializeLambda$(SerializedLambda l) {
  *   try return indy[scala.runtime.LambdaDeserialize.bootstrap, targetMethodGroup$0](l)
  *   catch {
  *     case i: IllegalArgumentException =>
  *       try return indy[scala.runtime.LambdaDeserialize.bootstrap, targetMethodGroup$1](l)
  *       catch {
  *         case i: IllegalArgumentException =>
  *           ...
  *             return indy[scala.runtime.LambdaDeserialize.bootstrap, targetMethodGroup${NUM_GROUPS-1}](l)
  *       }
  *   }
  * }
  *
  * We use invokedynamic here to enable caching within the deserializer without needing to
  * host a static field in the enclosing class. This allows us to add this method to interfaces
  * that define lambdas in default methods.
  *
  * SI-10232 we can't pass arbitrary number of method handles to the final varargs parameter of the bootstrap
  * method due to a limitation in the JVM. Instead, we emit a separate invokedynamic bytecode for each group of target
  * methods.
  */
  private def addLambdaDeserialize(classNode: ClassNode): Unit = {
    val implMethodsArray = collectSerializableLambdas(classNode)
    if implMethodsArray.isEmpty then
      return

    import asm.Opcodes.*
    val cw = classNode
    // Make sure to reference the ClassBTypes of all types that are used in the code generated
    // here (e.g. java/util/Map) are initialized. Initializing a ClassBType adds it to
    // `classBTypeFromInternalNameMap`. When writing the classfile, the asm ClassWriter computes
    // stack map frames and invokes the `getCommonSuperClass` method. This method expects all
    // ClassBTypes mentioned in the source code to exist in the map.
    val serializedLambdaObjDesc = s"(Ljava/lang/invoke/SerializedLambda;)L${ClassBType.javaLangObjectInternalName};"
    val mv = cw.visitMethod(ACC_PRIVATE + ACC_STATIC + ACC_SYNTHETIC, "$deserializeLambda$", serializedLambdaObjDesc, null, null)
    def emitLambdaDeserializeIndy(targetMethods: Seq[Handle]): Unit = {
      mv.visitVarInsn(ALOAD, 0)
      mv.visitInvokeDynamicInsn("lambdaDeserialize", serializedLambdaObjDesc, bTypes.jliLambdaDeserializeBootstrapHandle, targetMethods*)
    }

    val targetMethodGroupLimit = 255 - 1 - 3 // JVM limit. See MAX_MH_ARITY in CallSite.java
    val groups: Array[Array[Handle]] = implMethodsArray.grouped(targetMethodGroupLimit).toArray
    val numGroups = groups.length

    import scala.tools.asm.Label
    val initialLabels = Array.fill(numGroups - 1)(new Label())
    val terminalLabel = new Label
    def nextLabel(i: Int) = if (i == numGroups - 2) terminalLabel else initialLabels(i + 1)

    for ((label, i) <- initialLabels.iterator.zipWithIndex) {
      mv.visitTryCatchBlock(label, nextLabel(i), nextLabel(i), "java/lang/IllegalArgumentException")
    }
    for ((label, i) <- initialLabels.iterator.zipWithIndex) {
      mv.visitLabel(label)
      emitLambdaDeserializeIndy(groups(i).toIndexedSeq)
      mv.visitInsn(ARETURN)
    }
    mv.visitLabel(terminalLabel)
    emitLambdaDeserializeIndy(groups(numGroups - 1).toIndexedSeq)
    mv.visitInsn(ARETURN)
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
      val a = bTypeLoader.previouslyConstructedClassBType(inameA).get
      val b = bTypeLoader.previouslyConstructedClassBType(inameB).get
      val lub = a.jvmWiseLUB(b, bTypes.ObjectRef)
      val lubName = lub.internalName
      assert(lubName != "scala/Any")
      lubName // ASM caches the answer during the lifetime of a ClassWriter. We outlive that. Not sure whether caching on our side would improve things.
    }
  }
}

final class PostProcessorWithOptimizations(byteCodeRepository: BCodeRepository, bTypesFromClassfile: BTypesFromClassfile,
                                           callGraph: CallGraph, optimizerUtils: OptimizerUtils,
                                           bTypeLoader: BTypeLoader, bTypes: OptimizerKnownBTypes)(using Context) extends PostProcessor(bTypeLoader, bTypes) {
  private val optSettings         = new OptimizerSettings()
  private val closureOptimizer    = new ClosureOptimizer(optimizerUtils, byteCodeRepository, callGraph, bTypes, bTypesFromClassfile, optSettings)
  private val heuristics          = new InlinerHeuristics(optimizerUtils, byteCodeRepository, callGraph, bTypes, optSettings)
  private val inliner             = new Inliner(optimizerUtils, callGraph, bTypeLoader, bTypesFromClassfile, byteCodeRepository, heuristics, closureOptimizer, optSettings)
  private val localOpt            = new LocalOpt(optimizerUtils, callGraph, inliner, bTypes, bTypesFromClassfile, optSettings)

  override def runGlobalOptimizations(generatedUnits: Iterable[GeneratedCompilationUnit]): Unit = {
    // add classes to the bytecode repo before building the call graph: the latter needs to
    // look up classes and methods in the code repo.
    for u <- generatedUnits
        c <- u.classes
    do
      byteCodeRepository.add(c.classNode, Some(u.sourceFile.canonicalPath))
    for u <- generatedUnits
        c <- u.classes
        if !c.isArtifact // skip call graph for mirror / bean: we don't inline into them, and they are not referenced from other classes
    do
      callGraph.addClass(c.classNode)
    inliner.runInlinerAndClosureOptimizer(i => report.optimizerWarning(i.msg, i.site, i.pos))
  }

  protected override def runLocalOptimizations(classNode: ClassNode): Unit =
    localOpt.methodOptimizations(classNode)
}

/**
 * The result of code generation. [[isArtifact]] is `true` for mirror.
 */
case class GeneratedClass(
  classNode: ClassNode,
  sourceClassName: String,
  position: SourcePosition,
  isArtifact: Boolean,
  onFileCreated: AbstractFile => Unit)
case class GeneratedTasty(classNode: ClassNode, tastyGen: () => Array[Byte])
case class GeneratedCompilationUnit(sourceFile: AbstractFile, classes: List[GeneratedClass], tasty: List[GeneratedTasty])

