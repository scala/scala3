import scala.annotation.{experimental, MacroAnnotation}
import scala.quoted._
import scala.collection.mutable

@experimental
class memoize extends MacroAnnotation:
  def transform(using Quotes)(tree: quotes.reflect.Definition): List[quotes.reflect.Definition] =
    import quotes.reflect._
    tree match
      case DefDef(name, TermParamClause(param :: Nil) :: Nil, tpt, Some(rhsTree)) =>
        (Ref(param.symbol).asExpr, rhsTree.asExpr) match
          case ('{ $paramRefExpr: t }, '{ $rhsExpr: u }) =>
            val cacheSymbol = Symbol.newUniqueVal(tree.symbol.owner, name + "Cache", TypeRepr.of[mutable.Map[t, u]], Flags.Private, Symbol.noSymbol)
            val cacheRhs = '{ mutable.Map.empty[t, u] }.asTerm
            val cacheVal = ValDef(cacheSymbol, Some(cacheRhs))
            val cacheRefExpr = Ref(cacheSymbol).asExprOf[mutable.Map[t, u]]
            val newRhs = '{ $cacheRefExpr.getOrElseUpdate($paramRefExpr, $rhsExpr) }.asTerm
            val newTree = DefDef.copy(tree)(name, TermParamClause(param :: Nil) :: Nil, tpt, Some(newRhs))
            List(cacheVal, newTree)
      case _ =>
        report.error("Annotation only supported on `def` with a single argument are supported")
        List(tree)
