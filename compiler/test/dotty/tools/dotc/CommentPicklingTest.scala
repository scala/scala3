package dotty.tools.dotc

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Comments.CommentsContext
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Decorators.PreNamedString
import dotty.tools.dotc.core.Mode
import dotty.tools.dotc.core.Names.Name
import dotty.tools.dotc.core.tasty.DottyUnpickler
import dotty.tools.dotc.interfaces.Diagnostic.ERROR
import dotty.tools.dotc.reporting.TestReporter

import dotty.tools.vulpix.{TestConfiguration, ParallelTesting}

import java.io.IOException
import java.nio.file.{FileSystems, FileVisitOption, FileVisitResult, FileVisitor, Files, Path}
import java.nio.file.attribute.BasicFileAttributes
import java.util.EnumSet

import scala.concurrent.duration.{Duration, DurationInt}

import org.junit.Test
import org.junit.Assert.{assertEquals, assertFalse, fail}

class CommentPicklingTest extends ParallelTesting {

  override def isInteractive: Boolean = false
  override def testFilter: Option[String] = None
  override def maxDuration: Duration = 30.seconds
  override def numberOfSlaves: Int = 5
  override def safeMode: Boolean = false

  val compileOptions = TestConfiguration.defaultOptions and "-Ykeep-comments" and "-Yemit-tasty"
  val unpickleOptions = TestConfiguration.defaultOptions

  @Test def commentOnDef: Unit = {
    val sources = "object A { /** foo */ def bar = 2 }" :: Nil
    compileAndCheckComment(sources, "bar".toTermName, Some("/** foo */"))
  }

  @Test def commentOnVal: Unit = {
    val sources = "object A { /** foo */ val bar = 2 }" :: Nil
    compileAndCheckComment(sources, "bar".toTermName, Some("/** foo */"))
  }

  @Test def commentLocalVal: Unit = {
    val sources = "object A { def buzz = { /** foo */ val bar = 3 } }" :: Nil
    compileAndCheckComment(sources, "bar".toTermName, Some("/** foo */"))
  }

  @Test def commentLocalDef: Unit = {
    val sources = "object A { def buzz = { /** foo */ def bar = 5 } }" :: Nil
    compileAndCheckComment(sources, "bar".toTermName, Some("/** foo */"))
  }

  @Test def commentOnClass: Unit = {
    val sources = "/** foo */ class A" :: Nil
    compileAndCheckComment(sources, "A".toTypeName, Some("/** foo */"))
  }

  @Test def commentOnObject: Unit = {
    val sources = "/** foo */ object A" :: Nil
    compileAndCheckComment(sources, "A".toTermName, Some("/** foo */"))
  }

  @Test def commentOnlazyVal: Unit = {
    val sources = "class A { /** foo */ lazy val buzz = 2 }" :: Nil
    compileAndCheckComment(sources, "buzz".toTermName, Some("/** foo */"))
  }

  private def compileAndCheckComment(sources: Seq[String], treeName: Name, expectedComment: Option[String]): Unit = {
    compileAndUnpickle(sources) { (trees, ctx) =>
      findTreeNamed(treeName)(trees, ctx) match {
        case Some(md: tpd.MemberDef) =>
          val symbol = md.symbol(ctx)
          val comment = for { docCtx <- ctx.docCtx
                              comment <- docCtx.docstring(symbol) } yield comment.raw
          assertEquals(expectedComment, comment)
        case other =>
          fail(s"Unexpected: $other")
      }
    }
  }

  private def findTreeNamed(name: Name)(trees: Seq[tpd.Tree], ctx: Context): Option[tpd.NameTree] = {
    val acc = new tpd.TreeAccumulator[Option[tpd.NameTree]] {
      override def apply(x: Option[tpd.NameTree], tree: tpd.Tree)(implicit ctx: Context): Option[tpd.NameTree] = {
        x.orElse(tree match {
          case md: tpd.NameTree if md.name == name => Some(md)
          case md: tpd.NameTree => foldOver(None, md)
          case other => foldOver(None, other)
        })
      }
    }
    acc(None, trees)(ctx)
  }

  private def compileAndUnpickle[T](sources: Seq[String])(fn: (Seq[tpd.Tree], Context) => T) = {
    inTempDirectory { tmp =>
      val sourceFiles = sources.zipWithIndex.map {
        case (src, id) =>
          val path = tmp.resolve(s"Src$id.scala").toAbsolutePath
          Files.write(path, src.getBytes("UTF-8"))
          path.toString
      }

      val out = tmp.resolve("out")
      Files.createDirectories(out)

      val options = compileOptions.and("-d", out.toAbsolutePath.toString).and(sourceFiles: _*)
      val driver = new Driver
      val reporter = TestReporter.reporter(System.out, logLevel = ERROR)
      driver.process(options.all, reporter)
      assertFalse("Compilation failed.", reporter.hasErrors)

      val tastyFiles = getAll(tmp, "glob:**.tasty")
      val unpicklingOptions = unpickleOptions
        .withClasspath(out.toAbsolutePath.toString)
        .and("dummy") // Need to pass a dummy source file name
      val unpicklingDriver = new UnpicklingDriver
      unpicklingDriver.unpickle(unpicklingOptions.all, tastyFiles)(fn)
    }
  }

  private class UnpicklingDriver extends Driver {
    override def initCtx = super.initCtx.addMode(Mode.ReadComments)
    def unpickle[T](args: Array[String], paths: Seq[Path])(fn: (Seq[tpd.Tree], Context) => T): T = {
      implicit val (_, ctx: Context) = setup(args, initCtx)
      ctx.initialize()
      val trees = paths.flatMap { p =>
        val bytes = Files.readAllBytes(p)
        val unpickler = new DottyUnpickler(bytes)
        unpickler.enter(roots = Set.empty)
        unpickler.trees(ctx)
      }
      fn(trees, ctx)
    }
  }

  private def inTempDirectory[T](fn: Path => T): T = {
    val temp = Files.createTempDirectory("temp")
    try fn(temp)
    finally {
      val allFiles = getAll(temp, "glob:**").sortBy(_.toAbsolutePath.toString).reverse
      allFiles.foreach(Files.delete(_))
    }
  }

  private def getAll(base: Path,
             pattern: String,
             maxDepth: Int = Int.MaxValue): Seq[Path] = {
    val out = collection.mutable.ListBuffer.empty[Path]
    val matcher = FileSystems.getDefault.getPathMatcher(pattern)
    val visitor = new FileVisitor[Path] {
      override def preVisitDirectory(directory: Path, attributes: BasicFileAttributes): FileVisitResult = {
        if (matcher.matches(directory)) out += directory
        FileVisitResult.CONTINUE
      }

      override def postVisitDirectory(directory: Path, exception: IOException): FileVisitResult =
        FileVisitResult.CONTINUE

      override def visitFile(file: Path, attributes: BasicFileAttributes): FileVisitResult = {
        if (matcher.matches(file)) out += file
        FileVisitResult.CONTINUE
      }

      override def visitFileFailed(file: Path, exception: IOException): FileVisitResult =
        FileVisitResult.CONTINUE
    }
    Files.walkFileTree(base,
                       EnumSet.of(FileVisitOption.FOLLOW_LINKS),
                       maxDepth,
                       visitor)
    out
  }

}
