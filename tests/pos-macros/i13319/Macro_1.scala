import scala.quoted._
object Macro:
  inline def apply[A]: Unit = ${impl[A]}

  private def impl[A: Type](using Quotes): Expr[String] =
    import quotes.reflect._
    val t = TypeRepr.of[A]
    Expr.ofList(t.baseClasses.drop(1).filter(_.flags.is(Flags.Trait)).map { baseSymbol =>
        t.memberType(baseSymbol).asType  match { case '[t] => 42}
        Expr("")
    })
    Expr("")
