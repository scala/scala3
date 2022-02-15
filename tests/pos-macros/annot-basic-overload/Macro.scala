import scala.annotation.experimental
import scala.quoted._
import scala.annotation.MacroAnnotation

@experimental
class add extends MacroAnnotation {
  override def transform(using Quotes)(tree: quotes.reflect.Definition): List[quotes.reflect.Definition] =
    import quotes.reflect._
    tree match
      case ClassDef(name, constr, parents, self, body) =>
        given Quotes = tree.symbol.asQuotes
        val defSym = Symbol.newMethod(tree.symbol, "foo",
          MethodType(List("n"))(_ => List(TypeTree.of[String].tpe), _ => TypeTree.of[String].tpe))
        val overloadedDef = DefDef(defSym,
          { case List(List(n)) => Some('{${n.asExprOf[String]} + "1"}.asTerm)})
        List(ClassDef.copy(tree)(name, constr, parents, self, overloadedDef +: body))
}
