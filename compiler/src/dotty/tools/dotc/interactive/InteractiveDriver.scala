package dotty.tools
package dotc
package interactive

import scala.language.unsafeNulls

import java.net.URI
import java.io._
import java.nio.file._
import java.nio.file.attribute.BasicFileAttributes
import java.nio.charset.StandardCharsets
import java.util.zip._

import scala.collection._
import scala.io.Codec

import dotty.tools.io.{ AbstractFile, VirtualFile }

import ast.{Trees, tpd}
import core._, core.Decorators._
import Contexts._, Names._, NameOps._, Symbols._, SymDenotations._, Trees._, Types._
import Denotations.staticRef
import classpath._
import reporting._
import util._

/** A Driver subclass designed to be used from IDEs */
class InteractiveDriver(val settings: List[String]) extends Driver {
  import tpd._

  override def sourcesRequired: Boolean = false

  private val myInitCtx: Context = {
    val rootCtx = initCtx.fresh.addMode(Mode.ReadPositions).addMode(Mode.Interactive)
    rootCtx.setSetting(rootCtx.settings.YretainTrees, true)
    rootCtx.setSetting(rootCtx.settings.YcookComments, true)
    rootCtx.setSetting(rootCtx.settings.YreadComments, true)
    val ctx = setup(settings.toArray, rootCtx) match
      case Some((_, ctx)) => ctx
      case None => rootCtx
    ctx.initialize()(using ctx)
    ctx
  }

  private var myCtx: Context = myInitCtx
  def currentCtx: Context = myCtx

  private val compiler: Compiler = new InteractiveCompiler

  private val myOpenedFiles = new mutable.LinkedHashMap[URI, SourceFile] {
    override def default(key: URI) = NoSource
  }
  def openedFiles: Map[URI, SourceFile] = myOpenedFiles

  private val myOpenedTrees = new mutable.LinkedHashMap[URI, List[SourceTree]] {
    override def default(key: URI) = Nil
  }
  def openedTrees: Map[URI, List[SourceTree]] = myOpenedTrees

  private val myCompilationUnits = new mutable.LinkedHashMap[URI, CompilationUnit]
  def compilationUnits: Map[URI, CompilationUnit] = myCompilationUnits

  // Presence of a file with one of these suffixes indicates that the
  // corresponding class has been pickled with TASTY.
  private val tastySuffix = ".tasty"

  // FIXME: All the code doing classpath handling is very fragile and ugly,
  // improving this requires changing the dotty classpath APIs to handle our usecases.
  // We also need something like sbt server-mode to be informed of changes on
  // the classpath.

  private val (zipClassPaths, dirClassPaths) = currentCtx.platform.classPath(using currentCtx) match {
    case AggregateClassPath(cps) =>
      // FIXME: We shouldn't assume that ClassPath doesn't have other
      // subclasses. For now, the only other subclass is JrtClassPath on Java
      // 9+, we can safely ignore it for now because it's only used for the
      // standard Java library, but this will change once we start supporting
      // adding entries to the modulepath.
      val zipCps = cps.collect { case cp: ZipArchiveFileLookup[?] => cp }
      val dirCps = cps.collect { case cp: JFileDirectoryLookup[?] => cp }
      (zipCps, dirCps)
    case _ =>
      (Seq(), Seq())
  }

  // Like in `ZipArchiveFileLookup` we assume that zips are immutable
  private val zipClassPathClasses: Seq[TypeName] = {
    val names = new mutable.ListBuffer[TypeName]
    for (cp <- zipClassPaths)
      classesFromZip(cp.zipFile, names)
    names
  }

  initialize()

  /**
   * The trees for all the source files in this project.
   *
   * This includes the trees for the buffers that are presently open in the IDE, and the trees
   * from the target directory.
   */
  def sourceTrees(using Context): List[SourceTree] = sourceTreesContaining("")

  /**
   * The trees for all the source files in this project that contain `id`.
   *
   * This includes the trees for the buffers that are presently open in the IDE, and the trees
   * from the target directory.
   */
  def sourceTreesContaining(id: String)(using Context): List[SourceTree] = {
    val fromBuffers = openedTrees.values.flatten.toList
    val fromCompilationOutput = {
      val classNames = new mutable.ListBuffer[TypeName]
      val output = ctx.settings.outputDir.value
      if (output.isDirectory)
        classesFromDir(output.jpath, classNames)
      else
        classesFromZip(output.file, classNames)
      classNames.flatMap { cls =>
        treesFromClassName(cls, id)
      }
    }
    (fromBuffers ++ fromCompilationOutput).distinct
  }

  /**
   * All the trees for this project.
   *
   * This includes the trees of the sources of this project, along with the trees that are found
   * on this project's classpath.
   */
  def allTrees(using Context): List[SourceTree] = allTreesContaining("")

  /**
   * All the trees for this project that contain `id`.
   *
   * This includes the trees of the sources of this project, along with the trees that are found
   * on this project's classpath.
   */
  def allTreesContaining(id: String)(using Context): List[SourceTree] = {
    val fromSource = openedTrees.values.flatten.toList
    val fromClassPath = (dirClassPathClasses ++ zipClassPathClasses).flatMap { cls =>
      treesFromClassName(cls, id)
    }
    (fromSource ++ fromClassPath).distinct
  }

  def run(uri: URI, sourceCode: String): List[Diagnostic] = run(uri, toSource(uri, sourceCode))

  def run(uri: URI, source: SourceFile): List[Diagnostic] = {
    import typer.ImportInfo._

    val previousCtx = myCtx
    try {
      val reporter =
        new StoreReporter(null) with UniqueMessagePositions with HideNonSensicalMessages

      val run = compiler.newRun(using myInitCtx.fresh.setReporter(reporter))
      myCtx = run.runContext.withRootImports

      given Context = myCtx

      myOpenedFiles(uri) = source

      run.compileSources(List(source))
      run.printSummary()
      val unit = if ctx.run.units.nonEmpty then ctx.run.units.head else ctx.run.suspendedUnits.head
      val t = unit.tpdTree
      cleanup(t)
      myOpenedTrees(uri) = topLevelTrees(t, source)
      myCompilationUnits(uri) = unit
      myCtx = myCtx.fresh.setPhase(myInitCtx.base.typerPhase)

      reporter.removeBufferedMessages
    }
    catch {
      case ex: FatalError  =>
        myCtx = previousCtx
        close(uri)
        Nil
    }
  }

  def close(uri: URI): Unit = {
    myOpenedFiles.remove(uri)
    myOpenedTrees.remove(uri)
    myCompilationUnits.remove(uri)
  }

  /**
   * The `SourceTree`s that define the class `className` and/or module `className`.
   *
   * @see SourceTree.fromSymbol
   */
  private def treesFromClassName(className: TypeName, id: String)(using Context): List[SourceTree] = {
    def trees(className: TypeName, id: String): List[SourceTree] = {
      val clsd = staticRef(className)
      clsd match {
        case clsd: ClassDenotation =>
          clsd.ensureCompleted()
          SourceTree.fromSymbol(clsd.symbol.asClass, id)
        case _ =>
          Nil
      }
    }
    trees(className, id) ::: trees(className.moduleClassName, id)
  }

  // FIXME: classfiles in directories may change at any point, so we retraverse
  // the directories each time, if we knew when classfiles changed (sbt
  // server-mode might help here), we could do cache invalidation instead.
  private def dirClassPathClasses: Seq[TypeName] = {
    val names = new mutable.ListBuffer[TypeName]
    dirClassPaths.foreach { dirCp =>
      val root = dirCp.dir.toPath
      classesFromDir(root, names)
    }
    names
  }

  /** Adds the names of the classes that are defined in `file` to `buffer`. */
  private def classesFromZip(file: File, buffer: mutable.ListBuffer[TypeName]): Unit = {
    val zipFile = new ZipFile(file)
    try {
      val entries = zipFile.entries()
      while (entries.hasMoreElements) {
        val entry = entries.nextElement()
        val name = entry.getName
        if name.endsWith(tastySuffix) then
          buffer += name.replace("/", ".").stripSuffix(tastySuffix).toTypeName
      }
    }
    finally zipFile.close()
  }

  /** Adds the names of the classes that are defined in `dir` to `buffer`. */
  private def classesFromDir(dir: Path, buffer: mutable.ListBuffer[TypeName]): Unit =
    try
      Files.walkFileTree(dir, new SimpleFileVisitor[Path] {
        override def visitFile(path: Path, attrs: BasicFileAttributes) = {
          if (!attrs.isDirectory) {
            val name = path.getFileName.toString
            if name.endsWith(tastySuffix) then
              buffer += dir.relativize(path).toString.replace("/", ".").stripSuffix(tastySuffix).toTypeName
          }
          FileVisitResult.CONTINUE
        }
      })
    catch {
      case _: NoSuchFileException =>
    }

  private def topLevelTrees(topTree: Tree, source: SourceFile): List[SourceTree] = {
    val trees = new mutable.ListBuffer[SourceTree]

    def addTrees(tree: Tree): Unit = tree match {
      case PackageDef(_, stats) =>
        stats.foreach(addTrees)
      case imp: Import =>
        trees += SourceTree(imp, source)
      case tree: TypeDef =>
        trees += SourceTree(tree, source)
      case _ =>
    }
    addTrees(topTree)

    trees.toList
  }

  /** Remove attachments and error out completers. The goal is to avoid
   *  having a completer hanging in a typed tree which can capture the context
   *  of a previous run. Note that typed trees can have untyped or partially
   *  typed children if the source contains errors.
   */
  private def cleanup(tree: tpd.Tree)(using Context): Unit = {
    val seen = mutable.Set.empty[tpd.Tree]
    def cleanupTree(tree: tpd.Tree): Unit = {
      seen += tree
      tree.foreachSubTree { t =>
        if (t.symbol.exists && t.hasType) {
          if (!t.symbol.isCompleted) t.symbol.info = UnspecifiedErrorType
          t.symbol.annotations.foreach { annot =>
            /* In some cases annotations are are used on themself (possibly larger cycles).
            *  This is the case with the java.lang.annotation.Target annotation, would end
            *  in an infinite loop while cleaning. The `seen` is added to ensure that those
            *  trees are not cleand twice.
            *  TODO: Find a less expensive way to check for those cycles.
            */
            if (annot.isEvaluated && !seen(annot.tree))
              cleanupTree(annot.tree)
          }
        }
        t.removeAllAttachments()
      }
    }
    cleanupTree(tree)
  }

  private def toSource(uri: URI, sourceCode: String): SourceFile = {
    val path = Paths.get(uri)
    val virtualFile = new VirtualFile(path.getFileName.toString, path.toString)
    val writer = new BufferedWriter(new OutputStreamWriter(virtualFile.output, StandardCharsets.UTF_8.name))
    writer.write(sourceCode)
    writer.close()
    new SourceFile(virtualFile, Codec.UTF8)
  }

  /**
   * Initialize this driver and compiler.
   *
   * This is necessary because an `InteractiveDriver` can be put to work without having
   * compiled anything (for instance, resolving a symbol coming from a different compiler in
   * this compiler). In those cases, an un-initialized compiler may crash (for instance if
   * late-compilation is needed).
   */
  private def initialize(): Unit = {
    val run = compiler.newRun(using myInitCtx.fresh)
    myCtx = run.runContext
    run.compileUnits(Nil, myCtx)
  }
}


object InteractiveDriver {
  def toUriOption(file: AbstractFile): Option[URI] =
    if (!file.exists)
      None
    else
      try
        // We don't use file.file here since it'll be null
        // for the VirtualFiles created by InteractiveDriver#toSource
        // TODO: To avoid these round trip conversions, we could add an
        // AbstractFile#toUri method and implement it by returning a constant
        // passed as a parameter to a constructor of VirtualFile
        Some(Paths.get(file.path).toUri)
      catch {
        case e: InvalidPathException =>
          None
      }
  def toUriOption(source: SourceFile): Option[URI] =
    if (!source.exists)
      None
    else
      toUriOption(source.file)
}
