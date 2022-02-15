import scala.annotation.experimental
import scala.quoted._
import scala.annotation.MacroAnnotation
import scala.collection.mutable.Map

@experimental
class foo extends MacroAnnotation {
  override def transform(using Quotes)(tree: quotes.reflect.Definition): List[quotes.reflect.Definition] =
    import quotes.reflect._
    val s = '{@hello def foo1(x: Int): Int = x + 1;()}.asTerm
    val fooDef = s.asInstanceOf[Inlined].body.asInstanceOf[Block].statements.head.asInstanceOf[DefDef]
    val hello = Ref(tree.symbol.owner.declaredFields("hello").head).asExprOf[String] // error
    tree match
      case DefDef(name, params, tpt, Some(t)) =>
        val rhs = '{
          ${t.asExprOf[String]} + $hello
        }.asTerm
        val newDef = DefDef.copy(tree)(name, params, tpt, Some(rhs))
        List(fooDef, newDef)
}
