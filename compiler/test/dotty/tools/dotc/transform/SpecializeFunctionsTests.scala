package dotty.tools
package dotc
package transform

import scala.language.unsafeNulls

import org.junit.Test

import dotty.tools.backend.jvm.DottyBytecodeTest

class SpecializeFunctionsTests extends DottyBytecodeTest {

  import scala.jdk.CollectionConverters._

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
          case m if m.name == "apply$mcIII$sp" =>
            assert(!hasInvokeStatic(m)) // should not call super specialized method
            m
          case m if m.name == "apply" => m
        }
        .map(_.name)
        .toList

      assert(
        apps.length == 3,
        s"Wrong number of specialized applys, actual length: ${apps.length} - $apps"
      )
      assert(apps.contains("apply"), "Func2 did not contain `apply` forwarder method")
      assert(apps.contains("apply$mcIII$sp"), "Func2 did not contain specialized apply")
    }
  }

  @Test def notSpecializeAbstractMethod = {
    val source =
      """|trait Vector extends (Int=>Int) {
         |  override def apply(i: Int): Int
         |}""".stripMargin

    checkBCode(source) { dir =>
      val apps =
        findClass("Vector", dir).methods.asScala.collect {
          case m if m.name == "apply$mcII$sp" => m
          case m if m.name == "apply" => m
        }
        .map(_.name)
        .toList

      assert(
        apps.length == 1,
        s"Wrong number of specialized applys, actual length: ${apps.length} - $apps"
      )
    }
  }

  @Test def noBoxingSpecFunction0 = {
    implicit val source: String =
      """|class Test {
         |  class Func0 extends Function0[Int] {
         |    def apply() = 1337
         |  }
         |
         |  (new Func0: Function0[Int])()
         |}""".stripMargin

    checkBCode(source) { dir =>
      assertNoBoxing("<init>", findClass("Test", dir).methods)
    }
  }

  @Test def boxingFunction1 = {
    implicit val source: String =
      """|class Test {
         |  class Func1 extends Function1[Char, Int] {
         |    def apply(c: Char) = c.toInt
         |  }
         |
         |  (new Func1: Function1[Char, Int])('c')
         |}""".stripMargin

    checkBCode(source) { dir =>
      // No specialization for Function1[Char, Int]
      assertBoxing("<init>", findClass("Test", dir).methods)
    }
  }

  @Test def noBoxingSpecFunction1 = {
    implicit val source: String =
      """|class Test {
         |  class Func1 extends Function1[Int, Int] {
         |    def apply(i: Int) = i + 1
         |  }
         |
         |  (new Func1: Function1[Int, Int])(1)
         |}""".stripMargin

    checkBCode(source) { dir =>
      assertNoBoxing("<init>", findClass("Test", dir).methods)
    }
  }

  @Test def noBoxingSpecFunction2 = {
    implicit val source: String =
      """|class Test {
         |  class Func2 extends Function2[Int, Int, Int] {
         |    def apply(i: Int, j: Int) = i + j
         |  }
         |
         |  (new Func2: Function2[Int, Int, Int])(1300, 37)
         |}""".stripMargin

    checkBCode(source) { dir =>
      assertNoBoxing("<init>", findClass("Test", dir).methods)
    }
  }

  @Test def boxingFunction2 = {
    implicit val source: String =
      """|class Test {
         |  class Func2 extends Function2[Char, Char, Char] {
         |    def apply(c1: Char, c2: Char) = c1
         |  }
         |
         |  (new Func2: Function2[Char, Char, Char])('c', 'd')
         |}""".stripMargin

    checkBCode(source) { dir =>
      // No specialization for Function2[Char, Char, Char]
      assertBoxing("<init>", findClass("Test", dir).methods)
    }
  }

  @Test def multipleParentsNoBoxing = {
    implicit val source: String =
      """|class Test {
         |  class Func01 extends Function0[Int] with Function1[Int, Int] {
         |    def apply(): Int = 0
         |    def apply(x: Int): Int = x
         |  }
         |  (new Func01: Function0[Int])()
         |  (new Func01: Function1[Int, Int])(1)
         |}""".stripMargin

    checkBCode(source) { dir =>
      assertNoBoxing("<init>", findClass("Test", dir).methods)
    }
  }

  @Test def multipleLevelInheritanceNoBoxing = {
    implicit val source: String =
      """|class Test {
         |  class Func1[T](fn: T => Int) extends Function1[T, Int] {
         |    def apply(x: T): Int = fn(x)
         |  }
         |  class Fn extends Func1(identity[Int])
         |  (new Fn: Function1[Int, Int])(123)
         |}""".stripMargin

    checkBCode(source) { dir =>
      assertNoBoxing("<init>", findClass("Test", dir).methods)
    }
  }

  @Test def lambdaNoBoxing1 = {
    implicit val source: String =
      """|class Test {
         |  val fn = (x: Int) => x + 1
         |  fn(2)
         |}""".stripMargin

    checkBCode(source) { dir =>
      assertNoBoxing("<init>", findClass("Test", dir).methods)
    }
  }

  @Test def lambdaNoBoxing2 = {
    implicit val source: String =
      """|class Test {
         |  def fn[T, U, V](op0: T => U, op1: U => V): T => V = (x: T) => op1(op0(x))
         |  val f0: Int => Double = _.toDouble
         |  val f1: Double => Int = _.toInt
         |  val id = fn(f0, f1)
         |  id(2)
         |}""".stripMargin

    checkBCode(source) { dir =>
      assertNoBoxing("<init>", findClass("Test", dir).methods)
    }
  }

  @Test def classWithFieldBoxing = {
    implicit val source: String =
      """|class Test {
         |  class Func0[T](x: T) extends Function0[T] {
         |    def apply(): T = x
         |  }
         |  (new Func0(2): Function0[Int])()
         |}""".stripMargin

    checkBCode(source) { dir =>
      // Boxing happens because of the field of `Func0`.
      assertBoxing("<init>", findClass("Test", dir).methods)
    }
  }

  @Test def passByNameNoBoxing = {
    implicit val source: String =
      """|class Test {
         |  def fn(x: => Int): Int = x
         |  fn(2)
         |}""".stripMargin

    checkBCode(source) { dir =>
      assertNoBoxing("fn", findClass("Test", dir).methods)
    }
  }
}
