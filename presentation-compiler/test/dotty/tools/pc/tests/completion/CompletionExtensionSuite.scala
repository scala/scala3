package dotty.tools.pc.tests.completion

import org.junit.Test
import dotty.tools.pc.base.BaseCompletionSuite

class CompletionExtensionSuite extends BaseCompletionSuite:

  @Test def `simple` =
    check(
      """|package example
         |
         |object enrichments:
         |  extension (num: Int)
         |    def incr: Int = num + 1
         |
         |def main = 100.inc@@
         |""".stripMargin,
      """|incr: Int (extension)
         |""".stripMargin
    )

  @Test def `simple2` =
    check(
      """|package example
         |
         |object enrichments:
         |  extension (num: Int)
         |    def incr: Int = num + 1
         |
         |def main = 100.i@@
         |""".stripMargin,
      """|incr: Int (extension)
         |""".stripMargin,
      filter = _.contains("(extension)")
    )

  @Test def `filter-by-type` =
    check(
      """|package example
         |
         |object enrichments:
         |  extension (num: Int)
         |    def incr: Int = num + 1
         |  extension (str: String)
         |    def identity: String = str
         |
         |def main = "foo".i@@
         |""".stripMargin,
      """|identity: String (extension)
         |""".stripMargin, // incr won't be available
      filter = _.contains("(extension)")
    )

  @Test def `filter-by-type-subtype` =
    check(
      """|package example
         |
         |class A
         |class B extends A
         |
         |object enrichments:
         |  extension (a: A)
         |    def doSomething: A = a
         |
         |def main = (new B).do@@
         |""".stripMargin,
      """|doSomething: A (extension)
         |""".stripMargin,
      filter = _.contains("(extension)")
    )

  @Test def `simple-edit` =
    checkEdit(
      """|package example
         |
         |object enrichments:
         |  extension (num: Int)
         |    def incr: Int = num + 1
         |
         |def main = 100.inc@@
         |""".stripMargin,
      """|package example
         |
         |import example.enrichments.incr
         |
         |object enrichments:
         |  extension (num: Int)
         |    def incr: Int = num + 1
         |
         |def main = 100.incr
         |""".stripMargin
    )

  @Test def `simple-edit-suffix` =
    checkEdit(
      """|package example
         |
         |object enrichments:
         |  extension (num: Int)
         |    def plus(other: Int): Int = num + other
         |
         |def main = 100.pl@@
         |""".stripMargin,
      """|package example
         |
         |import example.enrichments.plus
         |
         |object enrichments:
         |  extension (num: Int)
         |    def plus(other: Int): Int = num + other
         |
         |def main = 100.plus($0)
         |""".stripMargin
    )
