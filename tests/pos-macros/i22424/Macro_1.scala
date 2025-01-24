
import scala.quoted.*

object MockMaker:
  inline def inlineMock[T]: Unit = ${instance[T]}
  transparent inline def transparentInlineMock[T]: Unit = ${instance[T]}

  def instance[T: Type](using quotes: Quotes): Expr[Unit] =
    import quotes.reflect._
    val tpe = TypeRepr.of[T]
    val symbol = tpe.typeSymbol.methodMember("innerTraitInOptions").head
    tpe.memberType(symbol) match
      case mt @ MethodType(_, args, _) =>
        assert(args.head.typeSymbol != TypeRepr.of[Nothing].typeSymbol, "argument is incorrectly approximated")
        val shownType = mt.show
        val expectedType = "(x: m.Embedded#ATrait[scala.Predef.String, scala.Int])m.Embedded#ATrait[scala.Predef.String, scala.Int]"
        assert(shownType == expectedType, s"Incorrect type shown. Obtained: $shownType, Expected: $expectedType")
    '{()}

trait PolymorphicTrait {
  trait Embedded {
    trait ATrait[A, B]
    def innerTraitInOptions(x: ATrait[String, Int]): ATrait[String, Int]
  }
}
