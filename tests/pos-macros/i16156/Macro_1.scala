import scala.quoted.*

trait MyEncoder[T]:
  def encode: String

class Context:
  inline def summonMyEncoder[T]: String =
    ${ SummonEncoder.impl[T] }
  implicit val encoderInstance: MyEncoder[String] =
    new MyEncoder[String] { def encode = "blah" }
end Context

object SummonEncoder:
  def impl[T: Type](using Quotes) =
    import quotes.reflect.*
    Expr.summon[MyEncoder[T]] match
      case Some(enc) => '{ $enc.encode }
      case None      => report.errorAndAbort("can't do it")
