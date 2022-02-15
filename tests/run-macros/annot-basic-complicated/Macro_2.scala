import scala.annotation.experimental
import scala.quoted._
import scala.annotation.MacroAnnotation
import scala.collection.mutable.Map

@experimental
class hello extends MacroAnnotation {
  override def transform(using Quotes)(tree: quotes.reflect.Definition): List[quotes.reflect.Definition] =
    import quotes.reflect._
    given Quotes = tree.symbol.owner.asQuotes
    val s = '{@add("world") @newS("world", true) val helloString = "hello";()}.asTerm
    val newDef = s.asInstanceOf[Inlined].body.asInstanceOf[Block].statements.head.asInstanceOf[ValDef]
    List(newDef, tree)
}
