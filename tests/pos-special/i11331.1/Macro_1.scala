package i11331

import scala.quoted._

class I11331Class

  
object X:

 inline def process[T](inline f:T) = ${
   processImpl[T]('f)
 }

 def processImpl[T:Type](t:Expr[T])(using Quotes):Expr[T] =
   import quotes.reflect._
   t
