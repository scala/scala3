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
     val mt = MethodType(List("p"))(
                _ => List(TypeRepr.of[Int]),
                _ => TypeRepr.of[Int]
              )
     val etaExpanded = Lambda(Symbol.spliceOwner, mt, (owner, params) => {
             Block(
               List(Assign(Select.unique(ref.asTerm,"x"),Literal(IntConstant(1)))),
               Apply(fun0,params.map(_.asInstanceOf[Term]))
             )
     })
     val r = Apply(etaExpanded,List(Literal(IntConstant(2))))
     r.asExprOf[Int]
   }

}
