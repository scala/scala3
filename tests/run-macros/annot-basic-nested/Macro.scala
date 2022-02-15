import scala.annotation.experimental
import scala.quoted._
import scala.annotation.MacroAnnotation
import scala.collection.mutable.Map

@experimental
class hello extends MacroAnnotation {
  override def transform(using Quotes)(tree: quotes.reflect.Definition): List[quotes.reflect.Definition] =
    import quotes.reflect._
    val helloSymbol = Symbol.newVal(tree.symbol.owner, "hello", TypeRepr.of[String], Flags.EmptyFlags, Symbol.noSymbol)
    val helloVal = ValDef(helloSymbol, Some(Literal(StringConstant("Hello, World!"))))
    List(helloVal, tree)
}
