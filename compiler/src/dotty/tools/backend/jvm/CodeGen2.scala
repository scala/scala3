package dotty.tools.backend.jvm

import dotty.tools.backend.jvm.BTypes.InternalName
import dotty.tools.backend.jvm.opt.{GlobalOptimizer, LocalOptimizer}
import dotty.tools.dotc.ast.Trees.PackageDef
import dotty.tools.dotc.ast.tpd.{EmptyTree, Tree, TypeDef, ValDef}
import dotty.tools.dotc.report
import dotty.tools.dotc.core.Contexts.{Context, atPhase}
import dotty.tools.dotc.core.Decorators.em
import dotty.tools.dotc.core.Phases.{Phase, sbtExtractDependenciesPhase}
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Symbols.ClassSymbol
import dotty.tools.dotc.core.TypeError
import dotty.tools.dotc.core.tasty.TastyUnpickler
import dotty.tools.dotc.interfaces.CompilerCallback
import dotty.tools.dotc.profile.ProfiledThreadPool
import dotty.tools.dotc.sbt.interfaces.IncrementalCallback
import dotty.tools.dotc.sbt.ExtractDependencies
import dotty.tools.dotc.util.SourcePosition
import dotty.tools.io.FileWriters

import java.util.concurrent.Future
import scala.annotation.constructorOnly
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.tools.asm.ClassWriter
import scala.tools.asm.tree.ClassNode

final class CodeGen2(ownerPhase: Phase, gen: BCode, localOpt: Option[LocalOptimizer], globalOpt: Option[GlobalOptimizer])(using @constructorOnly initctx: Context):
  // Save the compiler callbacks to avoid having to capture a Context
  private val compilerCallback = initctx.compilerCallback
  private val incrementalCallback = initctx.incCallback
  // Keep track of written class names so we can report (potential) conflicts
  private val caseInsensitiveClassNames = new java.util.HashMap[String, (String, SourcePosition)]
  // Create the classfile writer now to avoid having to capture a Context
  private val classfileWriter = FileWriters.ClassfileWriter(
    initctx.settings.outputDir.value,
    initctx.settings.XmainClass.valueSetByUser,
    initctx.settings.XjarCompressionLevel.value,
    initctx.settings.Xdumpclasses.value
  )
  // Let the profiler create an executor in the multithreaded case, since individual threads need to be profiled
  private val executor = initctx.settings.YbackendParallelism.value match
    case 1 => SynchronousExecutorService()
    case n =>
      // The thread pool queue is limited in size. When it's full,
      // a new task is executed on the main thread, which provides back-pressure.
      // The queue size is large enough to ensure that running a task on the main thread does
      // not take longer than to exhaust the queue for the backend workers.
      val queueSize = initctx.settings.YbackendWorkerQueue.valueSetByUser.getOrElse(n * 2)
      ProfiledThreadPool.newExecutor(ownerPhase, initctx.profiler, n - 1, queueSize, "gen-class-handler")
  // Java's ExecutorService doesn't let us tell whether there is ongoing work, so we must keep track of that ourselves.
  // We anyway need to keep track of the path in order to show it in error messages if something went deeply wrong.
  private val submittedExecutions = ListBuffer.empty[(Future[Unit], String)]
  // If we are globally optimizing, we can only emit class nodes to files once we have them all
  private val pendingClassNodes = ListBuffer.empty[(ClassNode, ClassNodeMetadata)]

  /** Adds the context's compilation unit to the work queue of units to be generated. */
  def addCompilationUnit()(using ctx: Context): Unit = {
    def iterateTypeDefs(tree: Tree): List[TypeDef] = tree match
      case td: TypeDef => List(td)
      case PackageDef(_, stats) => stats.flatMap(iterateTypeDefs)
      case EmptyTree | _: ValDef => Nil

    // Begin by generating class nodes.
    // This must be done on the main thread because it requires a Context.
    // Once we have the class node(s), we can emit any necessary error or warning related to names,
    // but order is not deterministic so we enforce lexicographic order for error-reporting
    val generatedClassNodes =
      iterateTypeDefs(ctx.compilationUnit.tpdTree)
        .flatMap(generateClassNodes)
        .sortBy((cn, _) => cn.name)
        .tapEach((cn, meta) => warnCaseInsensitiveOverwrite(cn.name, meta.position))
    // If we are doing global optimizations, we must collect class nodes and wait until we have them all,
    // i.e., until `finish` is called.
    // Otherwise, we can already schedule their generation in background threads.
    globalOpt match
      case Some(_) =>
        pendingClassNodes ++= generatedClassNodes
      case None =>
        schedule(generatedClassNodes)
  }

  /** Ensures all work is finished, files have been generated, and resources have been freed. Only call once per instance. */
  def finish()(using ctx: Context): Unit = {
    // If we are running global optimizations, we haven't scheduled anything yet; run such optimizations first, then schedule everything.
    // Otherwise, we have scheduled everything already.
    globalOpt match
      case Some(opt) =>
        opt.run(
          pendingClassNodes.map((n, m) => (n, m.position.source.file.canonicalPath)),
          i => report.optimizerWarning(i.msg, i.site, i.pos)
        )
        schedule(pendingClassNodes)
      case None =>
        assert(pendingClassNodes.isEmpty)
    // At this point all we need to do is wait.
    for (submitted, path) <- submittedExecutions do
      try submitted.get()
      catch case ex: Exception => report.error(s"Error while emitting $path\n${ex.getMessage}")
    // Finally, once everything is done, we can free resources.
    classfileWriter.close()
  }

  // This method MUST NOT take a Context, since it schedules work for concurrent execution
  private def schedule(classNodes: Iterable[(ClassNode, ClassNodeMetadata)]): Unit = {
    for (classNode, metadata) <- classNodes do
      // We want to release memory for GC as soon as processing is done, even if the Future is still referenced
      val classNodeRef = scala.runtime.ObjectRef(classNode)
      val metadataRef = scala.runtime.ObjectRef(metadata)
      val future = executor.submit[Unit](() => {
        val serializedClassNode = serializeClassNode(classNodeRef.elem)
        writeSerializedClassNode(classNodeRef.elem, metadataRef.elem, serializedClassNode)
        classNodeRef.elem = null
        metadataRef.elem = null
      })
      submittedExecutions += ((future, metadata.position.source.path))
  }

  private def generateClassNodes(typeDef: TypeDef)(using ctx: Context): List[(ClassNode, ClassNodeMetadata)] = {
    val position = typeDef.sourcePos
    try
      // First, generate the class nodes; the mirror is only generated if needed.
      val mainClassNode = gen.genClassNode(typeDef)
      val mirrorClassNode = gen.genMirrorClassNode(typeDef)
      // If the type def represents a class, and we have TASTY available, we must emit a TASTY attribute.
      // We must also store the TASTY for later emitting.
      val serializedTasty = typeDef.symbol match
        case classSymbol: ClassSymbol =>
          ctx.compilationUnit.pickled.get(classSymbol) match
            case Some(func) =>
              val forced = func()
              val nodeWithTastyAttribute = mirrorClassNode.getOrElse(mainClassNode)
              addTastyUuidToClassNode(nodeWithTastyAttribute, forced)
              Some(forced)
            case None => None
        case _ => None
      // Precompute callback-related information,
      val (fullName, isLocal) = atPhase(sbtExtractDependenciesPhase) {
        (ExtractDependencies.classNameAsString(typeDef.symbol), typeDef.symbol.isLocal)
      }
      // and return the nodes decorated with this info.
      mirrorClassNode match {
        case Some(mirror) => List(
          (mainClassNode, ClassNodeMetadata(fullName, isLocal, position, None)),
          (mirror, ClassNodeMetadata(fullName, isLocal, position, serializedTasty))
        )
        case None => List(
          (mainClassNode, ClassNodeMetadata(fullName, isLocal, position, serializedTasty))
        )
      }
    catch
      case ex: TypeError =>
        report.error(s"Error while emitting ${ctx.compilationUnit.source}\n${ex.getMessage}", position)
        Nil
  }

  private def warnCaseInsensitiveOverwrite(name: String, pos: SourcePosition)(using Context): Unit =
    caseInsensitiveClassNames.putIfAbsent(name.toLowerCase, (name, pos)) match {
      case null => ()
      case (dupName, dupPos) =>
        val locationAddendum =
          if pos.source.path == dupPos.source.path then ""
          else s" (defined in ${dupPos.source.file.name})"
        if name == dupName then
          report.error(em"${dotted(name)} and ${dotted(dupName)} produce classes that overwrite one another", pos)
        else
          report.warning(
            em"""Generated class ${dotted(name)} differs only in case from ${dotted(dupName)}$locationAddendum.
                |  Such classes will overwrite one another on case-insensitive filesystems.""", pos)
    }

  private def addTastyUuidToClassNode(classNode: ClassNode, serializedTasty: Array[Byte]): Unit = {
    // TASTY attribute is created but only the UUID bytes are stored in it.
    // A TASTY attribute has length 16 if and only if the .tasty file exists.
    val uuidBytes = TastyUnpickler.getUuidBytes(serializedTasty)
    val dataAttr = BCodeUtils.createJAttribute(nme.TASTYATTR.mangledString, uuidBytes, 0, uuidBytes.length)
    classNode.visitAttribute(dataAttr)
  }

  private def serializeClassNode(classNode: ClassNode): Array[Byte] = {
    /** Visit the class node and collect all referenced nested classes. */
    def collectNestedClasses(): (Iterable[ClassBType], Iterable[ClassBType]) = {
      val c = new NestedClassesCollector[ClassBType](nestedOnly = true) {
        def declaredNestedClasses(internalName: InternalName): List[ClassBType] =
          gen.classBTypeCache().previouslyConstructedClassBType(internalName).get.info.nestedClasses

        def getClassIfNested(internalName: InternalName): Option[ClassBType] = {
          val c = gen.classBTypeCache().previouslyConstructedClassBType(internalName).get
          Option.when(c.isNestedClass)(c)
        }

        def raiseError(msg: String, sig: String, e: Option[Throwable]): Unit = {
          // don't crash on invalid generic signatures
        }
      }
      c.visit(classNode)
      (c.declaredInnerClasses, c.referredInnerClasses)
    }

    /**
     * Populates the InnerClasses JVM attribute with `refedInnerClasses`. See also the doc on inner
     * classes in BTypes.scala.
     *
     * `refedInnerClasses` may contain duplicates, need not contain the enclosing inner classes of
     * each inner class it lists (those are looked up and included).
     *
     * This method serializes in the InnerClasses JVM attribute in an appropriate order,
     * not necessarily that given by `refedInnerClasses`.
     */
    def addInnerClasses(declaredInnerClasses: Iterable[ClassBType], refedInnerClasses: Iterable[ClassBType]): Unit = {
      // sorting ensures nested classes are listed after their enclosing class thus satisfying the Eclipse Java compiler
      val allNestedClasses = new mutable.TreeSet[ClassBType]()(using Ordering.by(_.internalName))
      allNestedClasses ++= declaredInnerClasses
      refedInnerClasses.foreach(allNestedClasses ++= _.enclosingNestedClassesChain)
      for nestedClass <- allNestedClasses do
        // Extract the innerClassEntry - we know it exists, enclosingNestedClassesChain only returns nested classes.
        val Some(e) = nestedClass.innerClassAttributeEntry: @unchecked
        classNode.visitInnerClass(e.name, e.outerName, e.innerName, e.flags)
    }

    // First, ensure we fill the inner classes, as required by the JVM
    classNode.innerClasses.clear()
    val (declared, referred) = collectNestedClasses()
    addInnerClasses(declared, referred)
    // Then, do local optimizations on the class node if requested
    localOpt.foreach(opt => opt.run(classNode))
    // Finally, convert the class node to bytes
    val writer = new ClassWriterWithBTypeLub(gen.classBTypeCache())
    classNode.accept(writer)
    writer.toByteArray
  }

  private def writeSerializedClassNode(classNode: ClassNode, metadata: ClassNodeMetadata, serialized: Array[Byte]): Unit = {
    val dottedName = dotted(classNode.name)

    TraceUtils.traceSerializedClassIfRequested(dottedName, serialized)

    val writtenClassFile = classfileWriter.writeClass(dottedName, serialized)
    metadata.serializedTasty.foreach(classfileWriter.writeTasty(classNode.name, _))

    compilerCallback match
      case null => ()
      case cb => cb.onClassGenerated(metadata.position.source, writtenClassFile, dottedName)

    incrementalCallback match
      case null => ()
      case cb if metadata.isLocal => cb.generatedLocalClass(metadata.position.source, writtenClassFile.jpath)
      case cb if !cb.enabled() =>
        // callback is not enabled, so nonLocalClasses were not reported in ExtractAPI
        cb.generatedNonLocalClass(metadata.position.source, writtenClassFile.jpath, dottedName, metadata.fullName)
      case cb => ()
  }

  private def dotted(name: String): String =
    name.replace('/', '.')

/** Metadata required for callbacks */
private final class ClassNodeMetadata(val fullName: String, val isLocal: Boolean, val position: SourcePosition, val serializedTasty: Option[Array[Byte]])

/**
 * An `asm.ClassWriter` that uses `jvmWiseLUB()`
 * The internal name of the least common ancestor of the types given by inameA and inameB.
 * It's what ASM needs to know in order to compute stack map frames, http://asm.ow2.org/doc/developer-guide.html#controlflow
 * Background:
 * http://gallium.inria.fr/~xleroy/publi/bytecode-verification-JAR.pdf
 * http://comments.gmane.org/gmane.comp.java.vm.languages/2293
 * https://github.com/scala/bug/issues/3872
 */
private final class ClassWriterWithBTypeLub(cache: ClassBType.Cache) extends ClassWriter(ClassWriter.COMPUTE_MAXS | ClassWriter.COMPUTE_FRAMES) {
  private val objectBType = cache.previouslyConstructedClassBType(ClassBType.javaLangObjectInternalName).get

  override def getCommonSuperClass(inameA: String, inameB: String): String = {
    // All types that appear in a class node need to have their ClassBType cached,
    // i.e., have been loaded either from symbols or from class files.
    val a = cache.previouslyConstructedClassBType(inameA).get
    val b = cache.previouslyConstructedClassBType(inameB).get
    val lub = a.jvmWiseLUB(b, objectBType)
    val lubName = lub.internalName
    assert(lubName != "scala/Any")
    lubName // ASM caches the answer during the lifetime of a ClassWriter. We outlive that. Not sure whether caching on our side would improve things.
  }
}