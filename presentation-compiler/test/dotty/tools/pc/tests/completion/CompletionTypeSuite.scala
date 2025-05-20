package dotty.tools.pc.tests.completion

import dotty.tools.pc.base.BaseCompletionSuite

import org.junit.Test

class CompletionTypeSuite extends BaseCompletionSuite:

  @Test def `type-bound` =
    check(
      s"""|import java.nio.file.{FileSystem => FS}
          |
          |object O {
          |  class Foo[T] {
          |    def method[T <: FS](a: T) = ???
          |  }
          |  val foo = new Foo
          |  foo.met@@
          |}
          |""".stripMargin,
      s"""|method[T <: FS](a: T): Nothing
          |""".stripMargin
    )

  @Test def `long-types-match` =
    check(
      s"""|@main
          |def run =
          |  (1, 2).to@@
          |""".stripMargin,
      s"""|toString(): String
          |toArray: Array[Object]
          |toIArray: IArray[Object]
          |toList: List[Int]
          |productIterator: Iterator[Any]
          |asInstanceOf[X0]: X0
          |isInstanceOf[X0]: Boolean
          |""".stripMargin
    )

  @Test def `union-types` =
    check(
      s"""|
          |def itsAUnionType(): Int | String = ???
          |
          |def hello =
          |  its@@
          |
          |
          |""".stripMargin,
      s"""|itsAUnionType(): Int | String
          |""".stripMargin
    )
