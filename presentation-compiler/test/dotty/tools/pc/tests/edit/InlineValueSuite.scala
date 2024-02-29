package dotty.tools.pc.tests.edit

import java.net.URI

import scala.meta.internal.jdk.CollectionConverters.*
import scala.meta.internal.metals.CompilerOffsetParams
import scala.meta.internal.mtags.CommonMtagsEnrichments
import scala.meta.internal.pc.InlineValueProvider.Errors as InlineErrors
import scala.meta.pc.DisplayableException
import scala.language.unsafeNulls

import dotty.tools.pc.base.BaseCodeActionSuite
import dotty.tools.pc.utils.TextEdits

import org.eclipse.lsp4j as l
import org.junit.Test

class InlineValueSuite extends BaseCodeActionSuite with CommonMtagsEnrichments:

  @Test def `inline-local` =
    checkEdit(
      """|object Main {
         |  def u(): Unit = {
         |    val o: Int = 1
         |    val p: Int = <<o>> + 2
         |  }
         |}""".stripMargin,
      """|object Main {
         |  def u(): Unit = {
         |    val p: Int = 1 + 2
         |  }
         |}""".stripMargin
    )

  @Test def `inline-local-same-name` =
    checkEdit(
      """|object Main {
         |  val a = { val a = 1; val b = <<a>> + 1 }
         |}""".stripMargin,
      """|object Main {
         |  val a = { val b = 1 + 1 }
         |}""".stripMargin
    )

  @Test def `inline-local-same-name2` =
    checkEdit(
      """|object Main {
         |  val b = {
         |    val a = 1
         |    val b = <<a>> + 1
         |  }
         |  val a = 3
         |}""".stripMargin,
      """|object Main {
         |  val b = {
         |    val b = 1 + 1
         |  }
         |  val a = 3
         |}""".stripMargin
    )

  @Test def `inline-local-same-name3` =
    checkEdit(
      """|object Main {
         |  val b = {
         |    val <<a>> = 1
         |    val b = a + 1
         |  }
         |  val a = 3
         |  val g = a
         |}""".stripMargin,
      """|object Main {
         |  val b = {
         |    val b = 1 + 1
         |  }
         |  val a = 3
         |  val g = a
         |}""".stripMargin
    )

  @Test def `inline-all-local` =
    checkEdit(
      """|object Main {
         |  def u(): Unit = {
         |    val <<o>>: Int = 1
         |    val p: Int = o + 2
         |    val i: Int = o + 3
         |  }
         |}""".stripMargin,
      """|object Main {
         |  def u(): Unit = {
         |    val p: Int = 1 + 2
         |    val i: Int = 1 + 3
         |  }
         |}""".stripMargin
    )

  @Test def `inline-all-local-val` =
    checkEdit(
      """|object Main {
         |  val u(): Unit = {
         |    val <<o>>: Int = 1
         |    val p: Int = o + 2
         |    val i: Int = o + 3
         |  }
         |}""".stripMargin,
      """|object Main {
         |  val u(): Unit = {
         |    val p: Int = 1 + 2
         |    val i: Int = 1 + 3
         |  }
         |}""".stripMargin
    )

  @Test def `inline-local-brackets` =
    checkEdit(
      """|object Main {
         |  def u(): Unit = {
         |    val o: Int = 1 + 6
         |    val p: Int = 2 - <<o>>
         |    val k: Int = o
         |  }
         |}""".stripMargin,
      """|object Main {
         |  def u(): Unit = {
         |    val o: Int = 1 + 6
         |    val p: Int = 2 - (1 + 6)
         |    val k: Int = o
         |  }
         |}""".stripMargin
    )

  @Test def `inline-all-local-brackets` =
    checkEdit(
      """|object Main {
         |  def u(): Unit = {
         |    val h: Int = 9
         |    val <<o>>: Int = 1 + 6
         |    val p: Int = h - o
         |    val k: Int = o
         |  }
         |}""".stripMargin,
      """|object Main {
         |  def u(): Unit = {
         |    val h: Int = 9
         |    val p: Int = h - (1 + 6)
         |    val k: Int = 1 + 6
         |  }
         |}""".stripMargin
    )

  @Test def `inline-not-local` =
    checkEdit(
      """|object Main {
         |  val o: Int = 6
         |  val p: Int = 2 - <<o>>
         |}""".stripMargin,
      """|object Main {
         |  val o: Int = 6
         |  val p: Int = 2 - 6
         |}""".stripMargin
    )

  @Test def `inline-not-local-pkg` =
    checkEdit(
      """|package m
         |object Main {
         |  val o: Int = 6
         |  val p: Int = 2 - <<o>>
         |}""".stripMargin,
      """|package m
         |object Main {
         |  val o: Int = 6
         |  val p: Int = 2 - 6
         |}""".stripMargin
    )

  @Test def `lambda-apply` =
    checkEdit(
      """|object Main {
         |  def demo = {
         |    val plus1 = (x: Int) => x + 1
         |    println(<<plus1>>(1))
         |  }
         |}""".stripMargin,
      """|object Main {
         |  def demo = {
         |    println(((x: Int) => x + 1)(1))
         |  }
         |}""".stripMargin
    )

  @Test def `lambda-as-arg` =
    checkEdit(
      """|object Main {
         |  def demo = {
         |    val plus1 = (x: Int) => x + 1
         |    val plus2 = <<plus1>>
         |  }
         |}""".stripMargin,
      """|object Main {
         |  def demo = {
         |    val plus2 = (x: Int) => x + 1
         |  }
         |}""".stripMargin
    )

  @Test def `inline-all-not-local` =
    checkError(
      """|object Main {
         |  val <<o>>: Int = 6
         |  val p: Int = 2 - o
         |}""".stripMargin,
      InlineErrors.notLocal
    )

  @Test def `for-comprehension` =
    checkEdit(
      """|object Main {
         |val a =
         |  for {
         |    i <- List(1,2,3)
         |  } yield i + 1
         |val b = <<a>>.map(_ + 1)
         |}""".stripMargin,
      """|object Main {
         |val a =
         |  for {
         |    i <- List(1,2,3)
         |  } yield i + 1
         |val b = (for {
         |    i <- List(1,2,3)
         |  } yield i + 1).map(_ + 1)
         |}""".stripMargin
    )

  @Test def `bracktes-add` =
    checkEdit(
      """|object Main {
         |  val b = 1 + (2 + 3)
         |  val c = <<b>>
         |}""".stripMargin,
      """|object Main {
         |  val b = 1 + (2 + 3)
         |  val c = 1 + (2 + 3)
         |}""".stripMargin
    )

  // --- different possibilites of conflicts ----------
  @Test def `scoping` =
    checkError(
      """|package scala.net.com.ooo
         |object Demo {
         |  val j: Int = 5
         |  val f: Int = 4 - j
         |
         |  def m() = {
         |    val j = 10
         |    val z = <<f>> + 1
         |  }
         |}""".stripMargin,
      InlineErrors.variablesAreShadowed("scala.net.com.ooo.Demo.j")
    )

  @Test def `scoping-class` =
    checkError(
      """|class Demo {
         |  val j: Int = 5
         |  val f: Int = 4 - j
         |
         |  def m() = {
         |    val j = 10
         |    val z = <<f>> + 1
         |  }
         |}""".stripMargin,
      InlineErrors.variablesAreShadowed("Demo.j")
    )

  // Note: we do not check if summoned implicts change when inlining
  @Test def `scoping-implicit` =
    checkEdit(
      """|object Demo {
         |  implicit val b : Boolean = true
         |  def myF(implicit b : Boolean): Int = if(b) 0 else 1
         |  val f: Int = myF
         |
         |  def m() = {
         |    implicit val v : Boolean = false
         |    val z = <<f>>
         |  }
         |}""".stripMargin,
      """|object Demo {
         |  implicit val b : Boolean = true
         |  def myF(implicit b : Boolean): Int = if(b) 0 else 1
         |  val f: Int = myF
         |
         |  def m() = {
         |    implicit val v : Boolean = false
         |    val z = myF
         |  }
         |}""".stripMargin
    )

  @Test def `scoping-packages` =
    checkError(
      """|package a
         |object A {
         |  val aaa = 1
         |}
         |package b;
         |object Demo {
         |  import a.A.aaa
         |  val inl = aaa
         |  def m() = {
         |    val aaa = 3
         |    <<inl>>
         |  }
         |}""".stripMargin,
      InlineErrors.variablesAreShadowed("a.A.aaa")
    )

  @Test def `bad-scoping` =
    checkError(
      """|object Demo {
         |  def oo(j : Int) = {
         |    val m = j + 3
         |    def kk() = {
         |      val j = 0
         |      <<m>>
         |    }
         |  }
         |}""".stripMargin,
      InlineErrors.variablesAreShadowed("Demo.oo.j")
    )

  @Test def `bad-scoping-2` =
    checkError(
      """|class A {
         |  val k = 3
         |  val l = k + 2
         |  case class B() {
         |     val k = 5
         |     val m = <<l>> + 3
         |  }
         |}""".stripMargin,
      InlineErrors.variablesAreShadowed("A.k")
    )

  def checkEdit(
      original: String,
      expected: String,
      filename: String = "file:/A.scala"
  ): Unit =
    val edits = getInlineEdits(original, filename)
    val (code, _, _) = params(original)
    val obtained = TextEdits.applyEdits(code, edits)

    assertNoDiff(expected, obtained)

  def checkError(
      original: String,
      expectedError: String,
      filename: String = "file:/A.scala"
  ): Unit =
    try
      val edits = getInlineEdits(original, filename)
      val (code, _, _) = params(original)
      val obtained = TextEdits.applyEdits(code, edits)
      fail(s"""|No error found, obtained:
               |$obtained""".stripMargin)
    catch
      case e: Exception if (e.getCause match
            case _: DisplayableException => true
            case _ => false
          ) =>
        assertNoDiff(expectedError, e.getCause.getMessage)

  def getInlineEdits(
      original: String,
      filename: String
  ): List[l.TextEdit] =
    val (code, _, offset) = params(original)
    val result = presentationCompiler
      .inlineValue(
        CompilerOffsetParams(
          URI.create(filename),
          code,
          offset,
          cancelToken
        )
      )
      .get()
    result.asScala.toList
