import scala.quoted.*

enum MyEnum0:
  case Marked
  case Marked2(i: Int)

trait MyMarker

enum MyEnum(val value: String):
  case Marked extends MyEnum("marked") with MyMarker
  case Marked2(i: Int) extends MyEnum("marked") with MyMarker

inline def enumMacro: Unit = ${ enumMacroExpr }

private def enumMacroExpr(using Quotes): Expr[Unit] =
  import quotes.reflect.*
  assert(TypeRepr.of[MyEnum0].typeSymbol.flags.is(Flags.Enum))
  assert(TypeRepr.of[MyEnum0.Marked.type].termSymbol.flags.is(Flags.Enum))
  assert(TypeRepr.of[MyEnum0.Marked2].typeSymbol.flags.is(Flags.Enum))
  assert(TypeRepr.of[MyEnum].typeSymbol.flags.is(Flags.Enum))
  assert(TypeRepr.of[MyEnum.Marked.type].termSymbol.flags.is(Flags.Enum))
  assert(TypeRepr.of[MyEnum.Marked2].typeSymbol.flags.is(Flags.Enum))

  '{}
