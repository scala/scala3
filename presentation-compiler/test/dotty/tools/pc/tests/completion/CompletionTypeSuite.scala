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
