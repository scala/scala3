import scala.annotation.experimental
import scala.quoted._
import scala.annotation.MacroAnnotation
import scala.collection.mutable.Map

@experimental
class hello extends MacroAnnotation {
  override def transformObject(using Quotes)(valTree: quotes.reflect.ValDef, classTree: quotes.reflect.TypeDef): List[quotes.reflect.Definition] =
    import quotes.reflect._
    val helloSymbol = Symbol.newVal(classTree.symbol.owner, "hello", TypeRepr.of[String], Flags.EmptyFlags, Symbol.noSymbol)
    val helloVal = ValDef(helloSymbol, Some(Literal(StringConstant("Hello, World!"))))
    List(helloVal, valTree, classTree)
}

@experimental
class double extends MacroAnnotation {
  override def transform(using Quotes)(tree: quotes.reflect.Definition): List[quotes.reflect.Definition] =
    import quotes.reflect._
    tree match
      case DefDef(name, params, tpt, Some(t)) =>
        val n = Ref(params.head.params.head.symbol).asExprOf[Int]
        val rhs = '{
          $n * 2
        }.asTerm
        val newDef = DefDef.copy(tree)(name, params, tpt, Some(rhs))
        List(newDef)
}
