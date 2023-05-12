package dotty.tools.pc.tests.highlight

import dotty.tools.pc.base.BaseDocumentHighlightSuite

import org.junit.Test

class SyntheticsDocumentHighlightSuite extends BaseDocumentHighlightSuite:

  @Test def `advanced1` =
    check(
      """
        |object Main {
        |  for {
        |    abc <- Option(1)
        |    one = 1
        |    <<a@@dd>> = one + abc
        |} yield {
        |   <<add>>.toString.toList.map(_.toChar)
        |  }
        |}""".stripMargin
    )

  @Test def `advanced2` =
    check(
      """
        |object Main {
        |  for {
        |    abc <- Option(1)
        |    one = 1
        |    <<add>> = one + abc
        |} yield {
        |   <<ad@@d>>.toString.toList.map(_.toChar)
        |  }
        |}""".stripMargin
    )

  @Test def `advanced3` =
    check(
      """
        |object Main {
        |  for {
        |    <<a@@bc>> <- Option(1)
        |    one = 1
        |    add = one + <<abc>>
        |} yield {
        |   <<abc>>.toString.toList.map(_.toChar)
        |  }
        |}""".stripMargin
    )

  @Test def `advanced4` =
    check(
      """
        |object Main {
        |  for {
        |    <<abc>> <- Option(1)
        |    one = 1
        |    add = one + <<a@@bc>>
        |} yield {
        |   <<abc>>.toString.toList.map(_.toChar)
        |  }
        |}""".stripMargin
    )

  @Test def `advanced5` =
    check(
      """
        |object Main {
        |  for {
        |    <<abc>> <- Option(1)
        |    one = 1
        |    add = one + <<abc>>
        |} yield {
        |   <<ab@@c>>.toString.toList.map(_.toChar)
        |  }
        |}""".stripMargin
    )
