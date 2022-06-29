package x

import scala.quoted.*


transparent inline def xtransform[T](inline expr:T) = ${
   X.transform('expr)
}

object X {

   def transform[T:Type](x: Expr[T])(using Quotes):Expr[T] = {
      import quotes.reflect.*
      x
   }

}