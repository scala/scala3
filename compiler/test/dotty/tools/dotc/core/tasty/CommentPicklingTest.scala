package dotty.tools.dotc.core.tasty

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.TreeOps
import dotty.tools.dotc.{Driver, Main}
import dotty.tools.dotc.core.Comments.CommentsContext
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Decorators.PreNamedString
import dotty.tools.dotc.core.Mode
import dotty.tools.dotc.core.Names.Name
import dotty.tools.dotc.interfaces.Diagnostic.ERROR
import dotty.tools.dotc.reporting.TestReporter
import dotty.tools.IOUtils

import dotty.tools.vulpix.TestConfiguration

import java.nio.file.{Files, Path}

import org.junit.Test
import org.junit.Assert.{assertEquals, assertFalse, fail}

class CommentPicklingTest {

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

  private def compileAndCheckComment(sources: List[String], treeName: Name, expectedComment: Option[String]): Unit = {
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

  private def findTreeNamed(name: Name)(trees: List[tpd.Tree], ctx: Context): Option[tpd.MemberDef] = {
    implicit val _ctx: Context = ctx
    trees.flatMap { _.find { case md: tpd.MemberDef => md.name == name; case _ => false }
      .map(_.asInstanceOf[tpd.MemberDef]).toList
    }.headOption
  }

  private def compileAndUnpickle[T](sources: List[String])(fn: (List[tpd.Tree], Context) => T) = {
    IOUtils.inTempDirectory { tmp =>
      val sourceFiles = sources.zipWithIndex.map {
        case (src, id) =>
          val path = tmp.resolve(s"Src$id.scala").toAbsolutePath
          Files.write(path, src.getBytes("UTF-8"))
          path.toString
      }

      val out = tmp.resolve("out")
      Files.createDirectories(out)

      val options = compileOptions.and("-d", out.toAbsolutePath.toString).and(sourceFiles: _*)
      val reporter = TestReporter.reporter(System.out, logLevel = ERROR)
      Main.process(options.all, reporter)
      assertFalse("Compilation failed.", reporter.hasErrors)

      val tastyFiles = IOUtils.getAll(tmp, "glob:**.tasty")
      val unpicklingOptions = unpickleOptions
        .withClasspath(out.toAbsolutePath.toString)
        .and("dummy") // Need to pass a dummy source file name
      val unpicklingDriver = new UnpicklingDriver
      unpicklingDriver.unpickle(unpicklingOptions.all, tastyFiles)(fn)
    }
  }

  private class UnpicklingDriver extends Driver {
    override def initCtx = super.initCtx.addMode(Mode.ReadComments)
    def unpickle[T](args: Array[String], paths: List[Path])(fn: (List[tpd.Tree], Context) => T): T = {
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

}
