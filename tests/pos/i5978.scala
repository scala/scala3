import scala.language.implicitConversions

opaque type Position[Buffer] = Int

trait TokenParser[Token, R]

package p1 {

  object TextParser {
    given TP as TokenParser[Char, Position[CharSequence]] {}

    def f
      given TokenParser[Char, Position[CharSequence]] = ???

    given FromCharToken as Conversion[Char, Position[CharSequence]]
      given (T: TokenParser[Char, Position[CharSequence]]) = ???
  }

  object Testcase {
    def main(args: Array[String]): Unit = {
      import given TextParser._
      import TextParser._

      val tp_v: TokenParser[Char, Position[CharSequence]] = TextParser.TP
      val tp_i = the[TokenParser[Char, Position[CharSequence]]]
      val co_i = the[Conversion[Char, Position[CharSequence]]]
      val co_x : Position[CharSequence] = 'x'

      {
        given XXX as Conversion[Char, Position[CharSequence]] = co_i
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
      import TextParser._
      import given TextParser._

      val tp_v: TokenParser[Char, Position[CharSequence]] = TextParser.TP
      val tp_i = the[TokenParser[Char, Position[CharSequence]]]
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
      import given TextParser._
      import TextParser._

      val co_i: Conversion[Char, Position[CharSequence]] = the[Conversion[Char, Position[CharSequence]]]

      {
        val tp_v: TokenParser[Char, Position[CharSequence]] = TextParser.TP
        val tp_i = the[TokenParser[Char, Position[CharSequence]]]
        given as Conversion[Char, Position[CharSequence]] = co_i
        val co_x : Position[CharSequence] = 'x'

        {
          given XXX as Conversion[Char, Position[CharSequence]] = co_i
          val co_y : Position[CharSequence] = 'x'
        }
      }
    }
  }
}
package p4 {
  class TC
  given A as TC
  given B[X[_], Y] as TC

  given C as TC
    given TC
}