package dotty.tools
package dotc
package transform

import org.junit.Assert._
import org.junit.Test

import dotty.tools.backend.jvm.DottyBytecodeTest

class SpecializeFunctionsTests extends DottyBytecodeTest {

  import dotty.tools.backend.jvm.ASMConverters._
  import dotty.tools.backend.jvm.AsmNode._
  import scala.collection.JavaConverters._
  import scala.tools.asm.tree.MethodNode

  @Test def specializeParentIntToInt = {
    val source = """
                 |class Foo extends Function1[Int, Int] {
                 |  def apply(i: Int) = i
                 |}
                 """.stripMargin

    checkBCode(source) { dir =>
      val applys =
        findClass("Foo", dir).methods.asScala.collect {
          case m if m.name == "apply$mcII$sp" => m
          case m if m.name == "apply" => m
        }
        .map(_.name)
        .toList

      assert(
        // there should be two "apply", one generic and the one overwritten and
        // then the specialized one
        applys.length == 3,
        s"Wrong number of specialized applys, actual length: ${applys.length} $applys"
      )
      assert(applys.contains("apply"), "Foo did not contain `apply` forwarder method")
      assert(applys.contains("apply$mcII$sp"), "Foo did not contain specialized apply")
    }
  }

  @Test def specializeFunction2Applys = {
    val source =
      """|class Func2 extends Function2[Int, Int, Int] {
         |    def apply(i: Int, j: Int): Int = i + j
         |}""".stripMargin

    checkBCode(source) { dir =>
      val apps =
        findClass("Func2", dir).methods.asScala.collect {
          case m if m.name == "apply$mcIII$sp" => m
          case m if m.name == "apply" => m
        }
        .map(_.name)
        .toList

      assert(
        apps.length == 3,
        s"Wrong number of specialized applys, actual length: ${apps.length} - $apps"
      )
      assert(apps.contains("apply"), "Func3 did not contain `apply` forwarder method")
      assert(apps.contains("apply$mcIII$sp"), "Func3 did not contain specialized apply")
    }
  }

  @Test def noBoxingSpecFunction0 = {
    implicit val source: String =
      """|object Test {
         |  class Func0 extends Function0[Int] {
         |    def apply() = 1337
         |  }
         |
         |  (new Func0: Function0[Int])()
         |}""".stripMargin

    checkBCode(source) { dir =>
      assertNoBoxing("<init>", findClass("Test$", dir).methods)
    }
  }

  @Test def boxingFunction1 = {
    implicit val source: String =
      """|object Test {
         |  class Func1 extends Function1[Char, Int] {
         |    def apply(c: Char) = c.toInt
         |  }
         |
         |  (new Func1: Function1[Char, Int])('c')
         |}""".stripMargin

    checkBCode(source) { dir =>
      assertBoxing("<init>", findClass("Test$", dir).methods)
    }
  }

  @Test def noBoxingSpecFunction1 = {
    implicit val source: String =
      """|object Test {
         |  class Func1 extends Function1[Int, Int] {
         |    def apply(i: Int) = i + 1
         |  }
         |
         |  (new Func1: Function1[Int, Int])(1)
         |}""".stripMargin

    checkBCode(source) { dir =>
      assertNoBoxing("<init>", findClass("Test$", dir).methods)
    }
  }

  @Test def noBoxingSpecFunction2 = {
    implicit val source: String =
      """|object Test {
         |  class Func2 extends Function2[Int, Int, Int] {
         |    def apply(i: Int, j: Int) = i + j
         |  }
         |
         |  (new Func2: Function2[Int, Int, Int])(1300, 37)
         |}""".stripMargin

    checkBCode(source) { dir =>
      assertNoBoxing("<init>", findClass("Test$", dir).methods)
    }
  }

  @Test def boxingFunction2 = {
    implicit val source: String =
      """|object Test {
         |  class Func2 extends Function2[Char, Char, Char] {
         |    def apply(c1: Char, c2: Char) = c1
         |  }
         |
         |  (new Func2: Function2[Char, Char, Char])('c', 'd')
         |}""".stripMargin

    checkBCode(source) { dir =>
      assertBoxing("<init>", findClass("Test$", dir).methods)
    }
  }
}
