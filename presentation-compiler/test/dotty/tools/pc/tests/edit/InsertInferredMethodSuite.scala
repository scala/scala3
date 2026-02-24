package dotty.tools.pc.tests.edit

import java.net.URI
import java.util.Optional

import scala.language.unsafeNulls
import scala.meta.internal.jdk.CollectionConverters.*
import scala.meta.internal.metals.CompilerOffsetParams
import scala.meta.pc.CodeActionId

import dotty.tools.pc.base.BaseCodeActionSuite
import dotty.tools.pc.utils.TextEdits

import org.eclipse.lsp4j as l
import org.junit.Test

class InsertInferredMethodSuite extends BaseCodeActionSuite:

  @Test def `simple` =
    checkEdit(
      """|
        |trait Main {
        |  def method1(s : String) = 123
        |
        |  method1(<<otherMethod>>(1))
        |}
        |
        |""".stripMargin,
      """|trait Main {
        |  def method1(s : String) = 123
        |
        |  def otherMethod(arg0: Int): String = ???
        |  method1(otherMethod(1))
        |}
        |""".stripMargin
    )

  @Test def `simple-2` =
    checkEdit(
      """|
        |trait Main {
        |  def method1(s : String) = 123
        |
        |  <<otherMethod>>(1)
        |}
        |
        |""".stripMargin,
      """|trait Main {
        |  def method1(s : String) = 123
        |
        |  def otherMethod(arg0: Int) = ???
        |  otherMethod(1)
        |}
        |""".stripMargin
    )

  @Test def `simple-3` =
    checkEdit(
      """|
        |trait Main {
        |  def method1(s : String) = 123
        |
        |  <<otherMethod>>((1 + 123).toDouble)
        |}
        |
        |""".stripMargin,
      """|trait Main {
        |  def method1(s : String) = 123
        |
        |  def otherMethod(arg0: Double) = ???
        |  otherMethod((1 + 123).toDouble)
        |}
        |""".stripMargin
    )

  @Test def `simple-4` =
    checkEdit(
      """|
        |trait Main {
        |  def method1(s : String) = 123
        |
        |  method1(<<otherMethod>>())
        |}
        |
        |""".stripMargin,
      """|trait Main {
        |  def method1(s : String) = 123
        |
        |  def otherMethod(): String = ???
        |  method1(otherMethod())
        |}
        |""".stripMargin
    )

  @Test def `backtick-method-name` =
    checkEdit(
      """|
        |trait Main {
        |  <<`met ? hod`>>(10)
        |}
        |""".stripMargin,
      """|trait Main {
        |  def `met ? hod`(arg0: Int) = ???
        |  `met ? hod`(10)
        |}
        |""".stripMargin
    )

  @Test def `custom-type` =
    checkEdit(
      """|
       |trait Main {
       |  def method1(b: Double, s : String) = 123
       |
       |  case class User(i : Int)
       |  val user = User(1)
       |
       |  method1(0.0, <<otherMethod>>(user, 1))
       |}
       |""".stripMargin,
      """|
       |trait Main {
       |  def method1(b: Double, s : String) = 123
       |
       |  case class User(i : Int)
       |  val user = User(1)
       |
       |  def otherMethod(arg0: User, arg1: Int): String = ???
       |  method1(0.0, otherMethod(user, 1))
       |}
       |""".stripMargin
    )

  @Test def `custom-type-2` =
    checkEdit(
      """|
       |trait Main {
       |  def method1(b: Double, s : String) = 123
       |
       |  case class User(i : Int)
       |  val user = User(1)
       |  <<otherMethod>>(user, 1)
       |}
       |""".stripMargin,
      """|
       |trait Main {
       |  def method1(b: Double, s : String) = 123
       |
       |  case class User(i : Int)
       |  val user = User(1)
       |  def otherMethod(arg0: User, arg1: Int) = ???
       |  otherMethod(user, 1)
       |}
       |""".stripMargin
    )

  @Test def `custom-type-advanced` =
    checkEdit(
      """|
       |trait Main {
       |    def method1(b: Double, s : String) = 123
       |
       |    case class User(i : Int)
       |
       |    <<otherMethod>>(User(1), 1)
       |}
       |
       |""".stripMargin,
      """|trait Main {
       |    def method1(b: Double, s : String) = 123
       |
       |    case class User(i : Int)
       |
       |    def otherMethod(arg0: User, arg1: Int) = ???
       |    otherMethod(User(1), 1)
       |}
       |""".stripMargin
    )

  @Test def `custom-type-advanced-2` =
    checkEdit(
      """|
       |trait Main {
       |    def method1(b: Double, s : String) = 123
       |
       |    case class User(i : Int)
       |
       |    <<otherMethod>>(List(Set(User(1))), Map("1" -> 1))
       |}
       |
       |""".stripMargin,
      """|trait Main {
       |    def method1(b: Double, s : String) = 123
       |
       |    case class User(i : Int)
       |
       |    def otherMethod(arg0: List[Set[User]], arg1: Map[String, Int]) = ???
       |    otherMethod(List(Set(User(1))), Map("1" -> 1))
       |}
       |""".stripMargin
    )

  @Test def `with-imports` =
    checkEdit(
      """|import java.nio.file.Files
       |
       |trait Main {
       |  def main() = {
       |    def method1(s : String) = 123
       |      val path = Files.createTempDirectory("")
       |      method1(<<otherMethod>>(path))
       |    }
       |}
       |
       |""".stripMargin,
      """|import java.nio.file.Files
       |import java.nio.file.Path
       |
       |trait Main {
       |  def main() = {
       |    def method1(s : String) = 123
       |      val path = Files.createTempDirectory("")
       |      def otherMethod(arg0: Path): String = ???
       |      method1(otherMethod(path))
       |    }
       |}
       |""".stripMargin
    )

  @Test def `val-definition` =
    checkEdit(
      """|
        |trait Main {
        |  val result: String = <<nonExistent>>(42, "hello")
        |}
        |
        |""".stripMargin,
      """|trait Main {
        |  def nonExistent(arg0: Int, arg1: String): String = ???
        |  val result: String = nonExistent(42, "hello")
        |}
        |""".stripMargin
    )

  @Test def `val-definition-no-args` =
    checkEdit(
      """|
        |trait Main {
        |  val result: Int = <<getValue>>
        |}
        |
        |""".stripMargin,
      """|trait Main {
        |  def getValue: Int = ???
        |  val result: Int = getValue
        |}
        |""".stripMargin
    )

  @Test def `lambda-expression` =
    checkEdit(
      """|
        |trait Main {
        |  val list = List(1, 2, 3)
        |  list.map(<<transform>>)
        |}
        |
        |""".stripMargin,
      """|trait Main {
        |  val list = List(1, 2, 3)
        |  def transform(arg0: Int) = ???
        |  list.map(transform)
        |}
        |""".stripMargin
    )

  @Test def `lambda-expression-2` =
    checkEdit(
      """|
        |trait Main {
        |  val list = List(1, 2, 3)
        |  list.map(<<transform>>(10, "test"))
        |}
        |
        |""".stripMargin,
      """|trait Main {
        |  val list = List(1, 2, 3)
        |  def transform(arg0: Int, arg1: String)(arg2: Int) = ???
        |  list.map(transform(10, "test"))
        |}
        |""".stripMargin
    )

  @Test def `lambda-expression-3` =
    checkEdit(
      """|
        |trait Main {
        |  val list = List("a", "b", "c")
        |  list.map(<<process>>)
        |}
        |
        |""".stripMargin,
      """|trait Main {
        |  val list = List("a", "b", "c")
        |  def process(arg0: String) = ???
        |  list.map(process)
        |}
        |""".stripMargin
    )

  @Test def `lambda-expression-4` =
    checkEdit(
      """|
        |trait Main {
        |  List((1, 2, 3)).filter(_ => true).map(<<otherMethod>>)
        |}
        |
        |""".stripMargin,
      """|trait Main {
        |  def otherMethod(arg0: (Int, Int, Int)) = ???
        |  List((1, 2, 3)).filter(_ => true).map(otherMethod)
        |}
        |""".stripMargin
    )

  @Test def `lambda-expression-5` =
    checkEdit(
      """|
        |trait Main {
        | val list = List(1, 2, 3)
        | list.filter(<<otherMethod>>)
        |}
        |
        |""".stripMargin,
      """|trait Main {
        | val list = List(1, 2, 3)
        | def otherMethod(arg0: Int): Boolean = ???
        | list.filter(otherMethod)
        |}
        |""".stripMargin
    )

  @Test def `simple-method-no-args` =
    checkEdit(
      """|
        |trait Main {
        |  <<missingMethod>>
        |}
        |
        |""".stripMargin,
      """|trait Main {
        |  def missingMethod = ???
        |  missingMethod
        |}
        |""".stripMargin
    )

  @Test def `simple-method-no-args-2` =
    checkEdit(
      """|
        |trait Main {
        |  def method1(s : String) = 123
        |  method1(<<missingMethod>>)
        |}
        |
        |""".stripMargin,
      """|trait Main {
        |  def method1(s : String) = 123
        |  def missingMethod: String = ???
        |  method1(missingMethod)
        |}
        |""".stripMargin
    )

  @Test def `nested-val-definition` =
    checkEdit(
      """|
        |trait Main {
        |  def someMethod(): Unit = {
        |    val data: List[String] = <<generateData>>(10)
        |  }
        |}
        |
        |""".stripMargin,
      """|trait Main {
        |  def someMethod(): Unit = {
        |    def generateData(arg0: Int): List[String] = ???
        |    val data: List[String] = generateData(10)
        |  }
        |}
        |""".stripMargin
    )

  @Test def `simple-class-definition` =
    checkEdit(
      """|
        |class User:
        |  val name: String = "John"
        |
        |object Main:
        |  val user = User()
        |  user.<<otherMethod>>
        |
        |""".stripMargin,
      """|
         |class User:
         |  val name: String = "John"
         |  def otherMethod = ???
         |
         |object Main:
         |  val user = User()
         |  user.otherMethod
         |""".stripMargin
    )

  @Test def `simple-class-definition-2` =
    checkEdit(
      """|
        |class User:
        |  val name: String = "John"
        |
        |object Main:
        |  val user = User()
        |  user.<<otherMethod>>(10)
        |
        |""".stripMargin,
      """|
         |class User:
         |  val name: String = "John"
         |  def otherMethod(arg0: Int) = ???
         |
         |object Main:
         |  val user = User()
         |  user.otherMethod(10)
         |""".stripMargin
    )

  @Test def `simple-object-definition` =
    checkEdit(
      """|
        |object User:
        |  val name: String = "John"
        |
        |object Main:
        |  User.<<otherMethod>>
        |
        |""".stripMargin,
      """|
         |object User:
         |  val name: String = "John"
         |  def otherMethod = ???
         |
         |object Main:
         |  User.otherMethod
         |""".stripMargin
    )

  @Test def `simple-object-definition-2` =
    checkEdit(
      """|
        |object User:
        |  val name: String = "John"
        |
        |object Main:
        |  User.<<otherMethod>>(10)
        |
        |""".stripMargin,
      """|
         |object User:
         |  val name: String = "John"
         |  def otherMethod(arg0: Int) = ???
         |
         |object Main:
         |  User.otherMethod(10)
         |""".stripMargin
    )

  @Test def `class-definition-without-body` =
    checkEdit(
      """|
        |class User
        |
        |object Main:
        |  val user = User()
        |  user.<<otherMethod>>
        |
        |""".stripMargin,
      """|
         |class User:
         |  def otherMethod = ???
         |
         |object Main:
         |  val user = User()
         |  user.otherMethod
         |""".stripMargin
    )

  @Test def `extension-method` =
    checkEdit(
      """|object Main:
         |  val x = 1
         |  x.<<incr>>
         |""".stripMargin,
      """|object Main:
         |  val x = 1
         |  extension (x: Int)
         |    def incr = ???
         |  x.incr
         |""".stripMargin
    )

  @Test def `extension-method-1` =
    checkEdit(
      """|object Main:
         |  val x = 1
         |  x.<<add>>(2)
         |""".stripMargin,
      """|object Main:
         |  val x = 1
         |  extension (x: Int)
         |    def add(arg0: Int) = ???
         |  x.add(2)
         |""".stripMargin
    )

  @Test def `extension-method-2` =
    checkEdit(
      """|object Main:
         |  val s = "hello"
         |  s.<<shout>>
         |""".stripMargin,
      """|object Main:
         |  val s = "hello"
         |  extension (x: String)
         |    def shout = ???
         |  s.shout
         |""".stripMargin
    )

  @Test def `extension-method-3` =
    checkEdit(
      """|import java.nio.file.Paths
         |
         |object Main:
         |  val p = Paths.get("test")
         |  p.<<newMethod>>
         |""".stripMargin,
      """|import java.nio.file.Paths
         |import java.nio.file.Path
         |
         |object Main:
         |  val p = Paths.get("test")
         |  extension (x: Path)
         |    def newMethod = ???
         |  p.newMethod
         |""".stripMargin
    )

  def checkEdit(
      original: String,
      expected: String
  ): Unit =
    val edits = getAutoImplement(original)
    val (code, _, _) = params(original)
    val obtained = TextEdits.applyEdits(code, edits)
    assertNoDiff(expected, obtained)

  def getAutoImplement(
      original: String,
      filename: String = "file:/A.scala"
  ): List[l.TextEdit] =
    val (code, _, offset) = params(original)
    val result = presentationCompiler
      .codeAction(
        CompilerOffsetParams(URI.create(filename), code, offset, cancelToken),
        CodeActionId.InsertInferredMethod,
        Optional.empty()
      )
      .get()
    result.asScala.toList
