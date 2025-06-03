package examples.select

import scala.language.dynamics
import scala.quoted.*

case class Wrapper[A](inner: A) extends Dynamic:
  transparent inline def selectDynamic(inline name: String) =
    ${selectDynamicImpl('inner, 'name)}

def selectDynamicImpl[A:Type](inner: Expr[A], name: Expr[String])(using quotes: Quotes): Expr[Any] =
  import quotes.reflect.*
  Select.unique(inner.asTerm, name.valueOrAbort).asExpr
