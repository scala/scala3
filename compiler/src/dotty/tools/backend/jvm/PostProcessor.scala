package dotty.tools.backend.jvm

import java.util.concurrent.ConcurrentHashMap
import dotty.tools.dotc.util.SourcePosition
import dotty.tools.io.AbstractFile
import dotty.tools.io.FileWriters
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Decorators.em

import scala.tools.asm.ClassWriter
import scala.tools.asm.tree.ClassNode
import dotty.tools.backend.jvm.opt.*
import dotty.tools.dotc.report
import dotty.tools.io.PlainFile.toPlainFile

import java.nio.file.{Files, Paths}
import scala.tools.asm
import scala.util.chaining.scalaUtilChainingOps

/**
 * Implements late stages of the backend, i.e.,
 * optimizations, post-processing and classfile serialization and writing.
 */
class PostProcessor(frontendAccess: PostProcessorFrontendAccess,
                    byteCodeRepository: BCodeRepository, bTypesFromClassfile: BTypesFromClassfile,
                    callGraph: CallGraph, backendUtils: BackendUtils,
                    bTypeLoader: BTypeLoader, bTypes: WellKnownBTypes)(using Context) {

  private val optSettings         = new OptimizerSettings()
  private val closureOptimizer    = new ClosureOptimizer(frontendAccess, backendUtils, byteCodeRepository, callGraph, bTypes, bTypesFromClassfile, optSettings)
  private val heuristics          = new InlinerHeuristics(frontendAccess, backendUtils, byteCodeRepository, callGraph, bTypes, optSettings)
  private val inliner             = new Inliner(frontendAccess, backendUtils, callGraph, bTypeLoader, bTypesFromClassfile, byteCodeRepository, heuristics, closureOptimizer, optSettings)
  private val localOpt            = new LocalOpt(backendUtils, callGraph, inliner, bTypes, bTypesFromClassfile, optSettings)

  given FileWriters.ReadOnlyContext = FileWriters.ReadOnlyContext.eager
  private val classfileWriter: FileWriters.ClassfileWriter = {
    val dumpClassesPath =
      ctx.settings.Xdumpclasses.valueSetByUser
        .map(p => Paths.get(p))
        .filter(path => Files.exists(path).tap(ok => if !ok then report.error(em"Output dir does not exist: ${path.toString}")))
        .map(_.toPlainFile)

    FileWriters.ClassfileWriter(ctx.settings.outputDir.value, ctx.settings.XmainClass.valueSetByUser, dumpClassesPath)
  }

  private type ClassnamePosition = (String, SourcePosition)
  private val caseInsensitively = new ConcurrentHashMap[String, ClassnamePosition]

  def sendToDisk(clazz: GeneratedClass): Unit = {
    val classNode = clazz.classNode
    val internalName = classNode.name.nn
    val bytes =
      try
        if !clazz.isArtifact then
          localOpt.methodOptimizations(classNode)
          setSerializableLambdas(classNode)
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

  def sendToDisk(tasty: GeneratedTasty): Unit = {
    val GeneratedTasty(classNode, tastyGenerator) = tasty
    val internalName = classNode.name.nn
    classfileWriter.writeTasty(classNode.name.nn, tastyGenerator())
  }

  def runGlobalOptimizations(generatedUnits: Iterable[GeneratedCompilationUnit]): Unit = {
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
    if ctx.settings.optInlineEnabled then
      inliner.runInlinerAndClosureOptimizer()
    else if ctx.settings.optClosureInvocations then
      closureOptimizer.rewriteClosureApplyInvocations(None, scala.collection.mutable.Map.empty)
  }

  def close(): Unit =
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

  private def setSerializableLambdas(classNode: ClassNode): Unit = {
    import backendUtils.{collectSerializableLambdas, addLambdaDeserialize}
    val serializableLambdas = collectSerializableLambdas(classNode)
    if serializableLambdas.nonEmpty then
      addLambdaDeserialize(classNode, serializableLambdas)
  }

  private def setInnerClasses(classNode: ClassNode): Unit = {
    classNode.innerClasses.nn.clear()
    val (declared, referred) = bTypeLoader.collectNestedClasses(classNode)
    backendUtils.addInnerClasses(classNode, declared, referred)
  }

  private def serializeClass(classNode: ClassNode): Array[Byte] = {
    val cw = new ClassWriterWithBTypeLub(asm.ClassWriter.COMPUTE_MAXS | asm.ClassWriter.COMPUTE_FRAMES)
    classNode.accept(cw)
    cw.toByteArray.nn
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
      val lub = a.jvmWiseLUB(b, bTypes)
      val lubName = lub.internalName
      assert(lubName != "scala/Any")
      lubName // ASM caches the answer during the lifetime of a ClassWriter. We outlive that. Not sure whether caching on our side would improve things.
    }
  }
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

