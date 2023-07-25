package x

import scala.quoted.*

class IntRef(var x: Int) {
   def plus(y:Int): Int = ???
}

object X {

   inline def test(ref:IntRef):Int = ${ testImpl('ref) }

   def testImpl(ref:Expr[IntRef])(using Quotes):Expr[Int] = {
     import quotes.reflect.*
     val fun0 = Select.unique(ref.asTerm,"plus")
     val fun1 = Block(List(Assign(Select.unique(ref.asTerm,"x"),Literal(IntConstant(1)))),fun0)
     val r = Apply(fun1,List(Literal(IntConstant(2))))
     r.asExprOf[Int]
   }

}
