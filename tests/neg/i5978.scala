import scala.language.implicitConversions

opaque type Position[Buffer] = Int

trait TokenParser[Token, R]

object TextParser {
  delegate TP for TokenParser[Char, Position[CharSequence]] {}

  delegate FromCharToken for Conversion[Char, Position[CharSequence]]
    given (T: TokenParser[Char, Position[CharSequence]])  = ???
}


object Testcase {
  def main(args: Array[String]): Unit = {
    import TextParser._

    val tp_v: TokenParser[Char, Position[CharSequence]] = TextParser.TP
    val tp_i = the[TokenParser[Char, Position[CharSequence]]] // error
    val co_i = the[Conversion[Char, Position[CharSequence]]]  // error
    val co_x : Position[CharSequence] = 'x'                   // error

    {
      delegate XXX for Conversion[Char, Position[CharSequence]] = co_i
      val co_y : Position[CharSequence] = 'x'
    }
  }
}