import scala.quoted.*

class Encoder[T]:
  def apply(element: T): List[String] = List(element.toString)

class EncoderContext { self =>
  def encode[Cls](cls: Cls) = List(cls.toString)
  implicit inline def anyClsEncoder[Cls]: Encoder[Cls] =
    MappedEncoderMaker[Cls](self)
}

object MappedEncoderMaker:
  inline def apply[T](inline ctx: EncoderContext): Encoder[T] = ${ applyImpl[T]('ctx) }
  def applyImpl[T: Type](ctx: Expr[EncoderContext])(using Quotes): Expr[Encoder[T]] =
    '{ new Encoder[T] { def encode(m: T) = $ctx.encode[T](m) } }

object SummonAndEncode {
  inline def apply[Cls <: AnyVal](cls: Cls): Unit = ${ applyImpl[Cls]('cls) }
  def applyImpl[Cls <: AnyVal: Type](cls: Expr[Cls])(using Quotes): Expr[Unit] = {
    import quotes.reflect.*
    Expr.summon[Encoder[Cls]] match
      case Some(value) => '{ () }
      case None => report.errorAndAbort("Encoder not found")
  }
}
