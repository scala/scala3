import scala.quoted.*

inline def gen: Unit = ${ genImpl }

def genImpl(using Quotes): Expr[Unit] = {
  import quotes.reflect.*

  val valDefSymbol = Symbol.newVal(Symbol.spliceOwner, "bar", TypeRepr.of[Unit], Flags.EmptyFlags, Symbol.spliceOwner)

  val valDef = ValDef(valDefSymbol, Some('{ () }.asTerm))

  Block(
    List(valDef),
    '{ () }.asTerm
  ).asExprOf[Unit]
}
