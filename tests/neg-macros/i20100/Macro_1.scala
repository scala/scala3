import scala.quoted.*

trait SelectableBase extends Selectable:
  val test: String = "a"
  transparent inline def selectDynamic(name: String): Any =
    ${selectDynamicImpl('this, 'name)}

def selectDynamicImpl[A:Type](inner: Expr[A], name: Expr[String])(using quotes: Quotes): Expr[Any] =
  import quotes.reflect.*
  Select.unique(inner.asTerm, "test").asExpr

def makeSelectable() =
  new SelectableBase(){ inline val age = 0 }
