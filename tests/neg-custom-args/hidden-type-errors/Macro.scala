package t12717

import scala.quoted._

object A:

   def foo(x:Int): Int = ???

   def bar(x:String): String = ???


object X:

   inline def doSomething[T](inline x:T):Any = ${
      doSomethingImpl('x)
   }

   def doSomethingImpl[T:Type](x:Expr[T])(using Quotes):Expr[Any] =
     import quotes.reflect._
     val aTerm = '{A}.asTerm
     val xBar = Apply(Select.unique(aTerm,"bar"),List(x.asTerm))
     Apply(Select.unique(aTerm,"foo"), List(xBar)).asExpr
