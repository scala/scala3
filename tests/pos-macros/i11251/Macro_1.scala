package i11251

import scala.quoted._

class SL:
  
  inline def foo(inline step: Int => Int): SL =
     val f = step
     this

  def run1(): Unit = ???
      
  inline def run(): Unit =
    run1()

// works
//object SL

object X:

 inline def process[T](inline f:T) = ${
   processImpl[T]('f)
 }

 def processImpl[T:Type](t:Expr[T])(using Quotes):Expr[T] = 
   import quotes.reflect._
   val checker = new TreeMap() {}
   checker.transformTerm(t.asTerm)(Symbol.spliceOwner)
   t
