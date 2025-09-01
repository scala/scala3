import scala.language.implicitConversions

case class Assign(left: String, right: String)
class SyntaxAnalyser extends ParsersBase {
  val x: Parser[String ~ String] = ???
  val y: Parser[Assign] = x.map(Assign.apply)
}

class ParsersBase {
  trait ~[+T, +U]
  abstract class Parser[+T]:
    def map[U](f: T => U): Parser[U] = ???

  given [A, B, X] => Conversion[(A, B) => X, (A ~ B) => X] = ???
}
