class Tokens { abstract class Token }
trait TokenParsers { val lexical: Tokens }


class MyTokenParsers extends TokenParsers {
  import lexical.*


  val lexical = new Tokens
}
