import scala.annotation.{experimental, MacroAnnotation}
import scala.quoted._
import scala.collection.mutable.Map

@experimental
class returnClassName extends MacroAnnotation {
  def transform(using Quotes)(tree: quotes.reflect.Definition): List[quotes.reflect.Definition] =
    import quotes.reflect._
    tree match
      case DefDef(name, params, tpt, _) =>
        val rhs = Literal(StringConstant(Symbol.spliceOwner.name.stripSuffix("$")))
        List(DefDef.copy(tree)(name, params, tpt, Some(rhs)))
}
