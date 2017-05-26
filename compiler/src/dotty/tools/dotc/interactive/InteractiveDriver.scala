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

  // FIXME: Change the Driver API to not require implementing this method
  override protected def newCompiler(implicit ctx: Context): Compiler = ???
  override def sourcesRequired = false

  private val myInitCtx: Context = {
    val rootCtx = initCtx.fresh.addMode(Mode.ReadPositions)
    rootCtx.setSetting(rootCtx.settings.YretainTrees, true)
    val ctx = setup(settings.toArray, rootCtx)._2
    ctx.initialize()(ctx)
    ctx
  }

  private var myCtx: Context = myInitCtx

  def currentCtx: Context = myCtx

  private val myOpenedFiles = new mutable.LinkedHashMap[URI, SourceFile]
  private val myOpenedTrees = new mutable.LinkedHashMap[URI, List[SourceTree]]

  def openedFiles: Map[URI, SourceFile] = myOpenedFiles
  def openedTrees: Map[URI, List[SourceTree]] = myOpenedTrees

  def allTrees(implicit ctx: Context): List[SourceTree] = {
    val fromSource = openedTrees.values.flatten.toList
    val fromClassPath = (dirClassPathClasses ++ zipClassPathClasses).flatMap { cls =>
      val className = cls.toTypeName
      List(tree(className), tree(className.moduleClassName)).flatten
    }
    (fromSource ++ fromClassPath).distinct
  }

  private def tree(className: TypeName)(implicit ctx: Context): Option[SourceTree] = {
    val clsd = ctx.base.staticRef(className)
    clsd match {
      case clsd: ClassDenotation =>
        SourceTree.fromSymbol(clsd.symbol.asClass)
      case _ =>
        sys.error(s"class not found: $className")
    }
  }

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
            val tastyFile = prefix + ".tasty"
            binFile match {
              case pf: PlainFile =>
                val tastyPath = pf.givenPath.parent / tastyFile
                tastyPath.exists
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
    entries.filter(_.getName.endsWith(".tasty"))
      .map(_.getName.replace("/", ".").stripSuffix(".tasty"))
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
          if (!attrs.isDirectory && path.getFileName.toString.endsWith(".tasty")) {
            names += root.relativize(path).toString.replace("/", ".").stripSuffix(".tasty")
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
      val t = run.units.head.tpdTree
      myOpenedTrees(uri) = topLevelClassTrees(t, source)

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
  }
}

object InteractiveDriver {
  def toUri(source: SourceFile) = Paths.get(source.file.path).toUri
}

