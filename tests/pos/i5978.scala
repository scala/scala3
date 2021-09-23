package test
import scala.language.implicitConversions

opaque type Position[Buffer] = Int

trait TokenParser[Token, R]

package p1 {

  object TextParser {
    given TP: TokenParser[Char, Position[CharSequence]] with {}

    def f
      (using TokenParser[Char, Position[CharSequence]]) = ???

    given FromCharToken(using T: TokenParser[Char, Position[CharSequence]])

      // skipping newlines is OK here

      : Conversion[Char, Position[CharSequence]] = ???
  }

  object Testcase {
    def main(args: Array[String]): Unit = {
      import TextParser.{given, *}

      val tp_v: TokenParser[Char, Position[CharSequence]] = TextParser.TP
      val tp_i = summon[TokenParser[Char, Position[CharSequence]]]
      val co_i = summon[Conversion[Char, Position[CharSequence]]]
      val co_x : Position[CharSequence] = 'x'

      {
        given XXX: Conversion[Char, Position[CharSequence]] = co_i
        val co_y : Position[CharSequence] = 'x'
      }
    }
  }
}
package p2 {

  object TextParser {
    implicit object TP extends TokenParser[Char, Position[CharSequence]] {}

    implicit def FromCharToken(c: Char)(implicit T: TokenParser[Char, Position[CharSequence]]): Position[CharSequence] = ???
  }

  object Testcase {
    def main(args: Array[String]): Unit = {
      import TextParser.{given, *}

      val tp_v: TokenParser[Char, Position[CharSequence]] = TextParser.TP
      val tp_i = summon[TokenParser[Char, Position[CharSequence]]]
      val co_x : Position[CharSequence] = 'x'
    }
  }
}
package p3 {

  object TextParser {
    implicit object TP extends TokenParser[Char, Position[CharSequence]] {}

    implicit def FromCharToken(implicit T: TokenParser[Char, Position[CharSequence]]): Conversion[Char, Position[CharSequence]] = ???
  }

  object Testcase {
    def main(args: Array[String]): Unit = {
      import TextParser.{_, given}

      val co_i: Conversion[Char, Position[CharSequence]] = summon[Conversion[Char, Position[CharSequence]]]

      {
        val tp_v: TokenParser[Char, Position[CharSequence]] = TextParser.TP
        val tp_i = summon[TokenParser[Char, Position[CharSequence]]]
        given Conversion[Char, Position[CharSequence]] = co_i
        val co_x : Position[CharSequence] = 'x'

        {
          given XXX: Conversion[Char, Position[CharSequence]] = co_i
          val co_y : Position[CharSequence] = 'x'
        }
      }
    }
  }
}
package p4 {
  class TC

  given A: TC with {}

  given B[X[_], Y]: TC with {}

  given C(using TC): TC with {}
}