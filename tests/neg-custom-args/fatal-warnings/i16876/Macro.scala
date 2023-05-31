import scala.quoted.*

def findMethodSymbol(using q: Quotes)(s: quotes.reflect.Symbol): quotes.reflect.Symbol =
  if s.isDefDef then
    s
  else
    findMethodSymbol(using q)(s.owner)
end findMethodSymbol


inline def adder: Int = ${
  adderImpl
}

def adderImpl(using q: Quotes): Expr[Int] =
  import quotes.reflect.*

  val inputs = findMethodSymbol(using q)(q.reflect.Symbol.spliceOwner).tree match
    case DefDef(_, params, _, _) =>
      params.last match
        case TermParamClause(valDefs) =>
          valDefs.map(vd => Ref(vd.symbol).asExprOf[Int])
  inputs.reduce((exp1, exp2) => '{ $exp1 + $exp2 })