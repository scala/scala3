import scala.annotation.experimental
import scala.quoted._
import scala.annotation.MacroAnnotation
import scala.collection.mutable.Map

@experimental
class memoize extends MacroAnnotation {
  override def transform(using Quotes)(tree: quotes.reflect.Definition): List[quotes.reflect.Definition] =
    import quotes.reflect._
    tree match
      case DefDef(name, params, tpt, Some(fibTree)) =>
        val cacheRhs = '{Map.empty[Int, Int]}.asTerm
        val cacheSymbol = Symbol.newVal(tree.symbol.owner, "fibCache", TypeRepr.of[Map[Int, Int]], Flags.EmptyFlags, Symbol.noSymbol)
        val cacheVal = ValDef(cacheSymbol, Some(cacheRhs))
        val fibCache = Ref(cacheSymbol).asExprOf[Map[Int, Int]]
        val n = Ref(params.head.params.head.symbol).asExprOf[Int]
        val rhs = '{
          if $fibCache.contains($n) then
              $fibCache($n)
          else
            val res = ${fibTree.asExprOf[Int]}
            $fibCache($n) = res
            res
        }.asTerm
        val newFib = DefDef.copy(tree)(name, params, tpt, Some(rhs))
        List(cacheVal, newFib)
}
