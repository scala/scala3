package dotty.tools
package repl

import scala.language.unsafeNulls

import java.io.File
import java.nio.file.Files

import org.junit.{ After, AfterClass, BeforeClass, Test }
import org.junit.Assert._
import io.{ Directory, PlainDirectory }
import dotc.core.Contexts._
import dotc.reporting.{ ErrorMessagesTest, StoreReporter }

object ShadowingBatchTests:
  val dir = Directory(Files.createTempDirectory("batch-shadow"))

  @BeforeClass def suiteStarting: Unit = dir.createDirectory()
  @AfterClass  def suiteFinished: Unit = dir.deleteRecursively()

class ShadowingBatchTests extends ErrorMessagesTest:
  import ShadowingBatchTests._

  @After def testFinished: Unit = dir.list.foreach(_.deleteRecursively())

  val compiler = new dotc.Compiler()

  override def initializeCtx(ictx: FreshContext) = inContext(ictx) {
    super.initializeCtx(ictx)
    val settings = ictx.settings; import settings._
    ictx.setSetting(outputDir, new PlainDirectory(dir))
    ictx.setSetting(classpath, classpath.value + File.pathSeparator + dir.jpath.toAbsolutePath)
  }

  @Test def io =
    val lib = """|package io.foo
                 |
                 |object Bar {
                 |  def baz: Int = 42
                 |}
                 |""".stripMargin
    val app = """|object Main:
                 |  def main(args: Array[String]): Unit =
                 |    println(io.foo.Bar.baz)
                 |""".stripMargin
    checkMessages(lib).expectNoErrors
    checkMessages(app).expectNoErrors

  @Test def file =
    checkMessages("class C(val c: Int)").expectNoErrors
    checkMessages("object rsline1 {\n  def line1 = new C().c\n}").expect { (_, msgs) =>
      assertMessageCount(1, msgs)
      assertEquals("missing argument for parameter c of constructor C in class C: (c: Int): C", msgs.head.message)
    }
    checkMessages("object rsline2 {\n  def line2 = new C(13).c\n}").expectNoErrors
    checkMessages("object rsline3 {\n  class C { val c = 42 }\n}").expectNoErrors
    checkMessages("import rsline3._\nobject rsline4 {\n  def line4 = new C().c\n}").expectNoErrors

  @Test def directory =
    checkMessages("package foo\nclass C").expectNoErrors
    checkMessages("object rsline1 {\n  def line1 = foo\n}").expect { (_, msgs) =>
      assertMessageCount(1, msgs)
      assertEquals("package foo is not a value", msgs.head.message)
    }
    checkMessages("object rsline2 {\n  val foo = 2\n}").expectNoErrors
    checkMessages("import rsline2._\nobject rsline3 {\n  def line3 = foo\n}").expectNoErrors

  @Test def directoryJava =
    checkMessages("object rsline1 {\n  def line1 = java\n}").expect { (_, msgs) =>
      assertMessageCount(1, msgs)
      assertEquals("package java is not a value", msgs.head.message)
    }
    checkMessages("object rsline2 {\n  val java = 2\n}").expectNoErrors
    checkMessages("import rsline2._\nobject rsline3 {\n  def line3 = java\n}").expectNoErrors

  def checkMessages(source: String): Report =
    ctx = newContext
    val run = compiler.newRun(using ctx.fresh)
    run.compileFromStrings(List(source))
    val runCtx = run.runContext
    if runCtx.reporter.hasErrors then
      val rep = runCtx.reporter.asInstanceOf[StoreReporter]
      val msgs = rep.removeBufferedMessages(using runCtx).map(_.msg).reverse
      new Report(msgs, runCtx)
    else new EmptyReport
end ShadowingBatchTests
