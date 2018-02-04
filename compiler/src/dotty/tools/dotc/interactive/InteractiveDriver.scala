package dotty.tools
package dotc
package interactive

import java.net.URI
import java.io._
import java.nio.file._
import java.nio.file.attribute.BasicFileAttributes
import java.util.stream._
import java.util.zip._
import java.util.function._

import scala.collection._
import JavaConverters._
import scala.io.Codec

import dotty.tools.io.{ ClassPath, ClassRepresentation, PlainFile, VirtualFile }

import ast.{Trees, tpd}
import core._, core.Decorators._
import Contexts._, Flags._, Names._, NameOps._, Symbols._, SymDenotations._, Trees._, Types._
import classpath._
import reporting._, reporting.diagnostic.MessageContainer
import util._

/** A Driver subclass designed to be used from IDEs */
class InteractiveDriver(settings: List[String]) extends Driver {
  import tpd._
  import InteractiveDriver._

  override def sourcesRequired = false

  private val myInitCtx: Context = {
    val rootCtx = initCtx.fresh.addMode(Mode.ReadPositions).addMode(Mode.Interactive)
    rootCtx.setSetting(rootCtx.settings.YretainTrees, true)
    val ctx = setup(settings.toArray, rootCtx)._2
    ctx.initialize()(ctx)
    ctx
  }

  private[this] var myCtx: Context = myInitCtx

  def currentCtx: Context = myCtx

  private val myOpenedFiles = new mutable.LinkedHashMap[URI, SourceFile] {
    override def default(key: URI) = NoSource
  }

  private val myOpenedTrees = new mutable.LinkedHashMap[URI, List[SourceTree]] {
    override def default(key: URI) = Nil
  }

  private val myCompilationUnits = new mutable.LinkedHashMap[URI, CompilationUnit]

  def openedFiles: Map[URI, SourceFile] = myOpenedFiles
  def openedTrees: Map[URI, List[SourceTree]] = myOpenedTrees
  def compilationUnits: Map[URI, CompilationUnit] = myCompilationUnits

  def allTrees(implicit ctx: Context): List[SourceTree] = allTreesContaining("")

  def allTreesContaining(id: String)(implicit ctx: Context): List[SourceTree] = {
    val fromSource = openedTrees.values.flatten.toList
    val fromClassPath = (dirClassPathClasses ++ zipClassPathClasses).flatMap { cls =>
      val className = cls.toTypeName
      List(tree(className, id), tree(className.moduleClassName, id)).flatten
    }
    (fromSource ++ fromClassPath).distinct
  }

  private def tree(className: TypeName, id: String)(implicit ctx: Context): Option[SourceTree] = {
    val clsd = ctx.base.staticRef(className)
    clsd match {
      case clsd: ClassDenotation =>
        clsd.ensureCompleted()
        SourceTree.fromSymbol(clsd.symbol.asClass, id)
      case _ =>
        None
    }
  }

  // Presence of a file with one of these suffixes indicates that the
  // corresponding class has been pickled with TASTY.
  private val tastySuffixes = List(".hasTasty", ".tasty")

  private def classNames(cp: ClassPath, packageName: String): List[String] = {
    def className(classSegments: List[String]) =
      classSegments.mkString(".").stripSuffix(".class")

    val ClassPathEntries(pkgs, classReps) = cp.list(packageName)

    classReps
      .filter((classRep: ClassRepresentation) => classRep.binary match {
        case None =>
          true
        case Some(binFile) =>
          val prefix =
            if (binFile.name.endsWith(".class"))
              binFile.name.stripSuffix(".class")
            else
              null
          prefix != null && {
            binFile match {
              case pf: PlainFile =>
                tastySuffixes.map(suffix => pf.givenPath.parent / (prefix + suffix)).exists(_.exists)
              case _ =>
                sys.error(s"Unhandled file type: $binFile [getClass = ${binFile.getClass}]")
            }
          }
      })
      .map(classRep => (packageName ++ (if (packageName != "") "." else "") ++ classRep.name)).toList ++
    pkgs.flatMap(pkg => classNames(cp, pkg.name))
  }

  // FIXME: All the code doing classpath handling is very fragile and ugly,
  // improving this requires changing the dotty classpath APIs to handle our usecases.
  // We also need something like sbt server-mode to be informed of changes on
  // the classpath.

  private val (zipClassPaths, dirClassPaths) = currentCtx.platform.classPath(currentCtx) match {
    case AggregateClassPath(cps) =>
      val (zipCps, dirCps) = cps.partition(_.isInstanceOf[ZipArchiveFileLookup[_]])
      // This will be wrong if any other subclass of ClassPath is either used,
      // like `JrtClassPath` once we get Java 9 support
      (zipCps.asInstanceOf[Seq[ZipArchiveFileLookup[_]]], dirCps.asInstanceOf[Seq[JFileDirectoryLookup[_]]])
    case _ =>
      (Seq(), Seq())
  }

  // Like in `ZipArchiveFileLookup` we assume that zips are immutable
  private val zipClassPathClasses: Seq[String] = zipClassPaths.flatMap { zipCp =>
    // Working with Java 8 stream without SAMs and scala-java8-compat is awful.
    val entries = new ZipFile(zipCp.zipFile)
      .stream
      .toArray(new IntFunction[Array[ZipEntry]] { def apply(size: Int) = new Array(size) })
      .toSeq
    for {
      entry <- entries
      name = entry.getName
      tastySuffix <- tastySuffixes
      if name.endsWith(tastySuffix)
    } yield name.replace("/", ".").stripSuffix(tastySuffix)
  }

  // FIXME: classfiles in directories may change at any point, so we retraverse
  // the directories each time, if we knew when classfiles changed (sbt
  // server-mode might help here), we could do cache invalidation instead.
  private def dirClassPathClasses: Seq[String] = {
    val names = new mutable.ListBuffer[String]
    dirClassPaths.foreach { dirCp =>
      val root = dirCp.dir.toPath
      Files.walkFileTree(root, new SimpleFileVisitor[Path] {
        override def visitFile(path: Path, attrs: BasicFileAttributes) = {
          if (!attrs.isDirectory) {
            val name = path.getFileName.toString
            for {
              tastySuffix <- tastySuffixes
              if name.endsWith(tastySuffix)
            } {
              names += root.relativize(path).toString.replace("/", ".").stripSuffix(tastySuffix)
            }
          }
          FileVisitResult.CONTINUE
        }
      })
    }
    names.toList
  }

  private def topLevelClassTrees(topTree: Tree, source: SourceFile): List[SourceTree] = {
    val trees = new mutable.ListBuffer[SourceTree]

    def addTrees(tree: Tree): Unit = tree match {
      case PackageDef(_, stats) =>
        stats.foreach(addTrees)
      case tree: TypeDef =>
        trees += SourceTree(tree, source)
      case _ =>
    }
    addTrees(topTree)

    trees.toList
  }

  private val compiler: Compiler = new InteractiveCompiler

  /** Remove attachments and error out completers. The goal is to avoid
   *  having a completer hanging in a typed tree which can capture the context
   *  of a previous run. Note that typed trees can have untyped or partially
   *  typed children if the source contains errors.
   */
  private def cleanup(tree: tpd.Tree)(implicit ctx: Context): Unit = {
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

  def run(uri: URI, sourceCode: String): List[MessageContainer] = {
    val previousCtx = myCtx
    try {
      val reporter =
        new StoreReporter(null) with UniqueMessagePositions with HideNonSensicalMessages

      val run = compiler.newRun(myInitCtx.fresh.setReporter(reporter))
      myCtx = run.runContext

      implicit val ctx = myCtx

      val virtualFile = new VirtualFile(uri.toString, Paths.get(uri).toString)
      val writer = new BufferedWriter(new OutputStreamWriter(virtualFile.output, "UTF-8"))
      writer.write(sourceCode)
      writer.close()
      val source = new SourceFile(virtualFile, Codec.UTF8)
      myOpenedFiles(uri) = source

      run.compileSources(List(source))
      run.printSummary()
      val unit = ctx.run.units.head
      val t = unit.tpdTree
      cleanup(t)
      myOpenedTrees(uri) = topLevelClassTrees(t, source)
      myCompilationUnits(uri) = unit

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
}

object InteractiveDriver {
  def toUri(source: SourceFile) = Paths.get(source.file.path).toUri
}

