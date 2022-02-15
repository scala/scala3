import scala.annotation.experimental
import scala.quoted._
import scala.annotation.MacroAnnotation
import scala.collection.mutable.Map

@experimental
class newS(s: String, up: Boolean) extends MacroAnnotation {
  override def transform(using Quotes)(tree: quotes.reflect.Definition): List[quotes.reflect.Definition] =
    import quotes.reflect._
    val newSymbol = Symbol.newVal(tree.symbol.owner, s, TypeRepr.of[String], Flags.EmptyFlags, Symbol.noSymbol)
    val newVal = ValDef(newSymbol, Some(Literal(StringConstant(s))))
    if up then List(newVal, tree) else List(tree, newVal)
}

@experimental
class add(s: String) extends MacroAnnotation {
  override def transform(using Quotes)(tree: quotes.reflect.Definition): List[quotes.reflect.Definition] =
    import quotes.reflect._
    val hello = Ref(tree.symbol.owner.declaredField(s)).asExprOf[String]

    tree match
      case DefDef(name, params, tpt, Some(t)) =>
        val rhs = '{
          ${t.asExprOf[String]} + $hello
        }.asTerm
        List(DefDef.copy(tree)(name, params, tpt, Some(rhs)))
      case ValDef(name, tpt, Some(t)) =>
        val rhs = '{
          ${t.asExprOf[String]} + $hello
        }.asTerm
        List(ValDef.copy(tree)(name, tpt, Some(rhs)))
}
