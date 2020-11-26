import scala.language.implicitConversions

opaque type Position[Buffer] = Int

trait TokenParser[Token, R]

object TextParser {
  given TokenParser[Char, Position[CharSequence]] as tp {}

  given (T: TokenParser[Char, Position[CharSequence]])
    => Conversion[Char, Position[CharSequence]]
  as FromCharToken = ???
}

object Testcase {
  def main(args: Array[String]): Unit = {
    import TextParser._

    val tp_v: TokenParser[Char, Position[CharSequence]] = TextParser.tp
    val tp_i = summon[TokenParser[Char, Position[CharSequence]]] // error
    val co_i = summon[Conversion[Char, Position[CharSequence]]]  // error
    val co_x : Position[CharSequence] = 'x'                   // error

    {
      given Conversion[Char, Position[CharSequence]] as xxx = co_i
      val co_y : Position[CharSequence] = 'x'
    }
  }
}