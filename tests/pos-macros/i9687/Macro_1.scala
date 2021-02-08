package x

import scala.quoted.*

object FastPath {

  inline def sum(x:Int):Int =
    x + 1

}

object SlowPath {

  def sum(x:Int):Int =
    x + 1

}

object X {

 inline def transform[A](inline x:A):A = ${
    transformImpl[A]('x)
 }

 def transformImpl[A:Type](x:Expr[A])(using Quotes):Expr[A] = {
    import quotes.reflect.*
    val slowPath = '{ SlowPath }.asTerm
    val fastPath = '{ FastPath }.asTerm
    val transformer = new TreeMap() {
      override def transformTerm(term:Term)(owner: Symbol):Term = {
        term match
          case Apply(sel@Select(o,m),args) =>
                if ( o.tpe =:= slowPath.tpe && m=="sum" )
                   Apply(Select.unique(fastPath,"sum"), args)
                else
                   super.transformTerm(term)(owner)
          case _ => super.transformTerm(term)(owner)
      }
    }
    val r = transformer.transformTerm(x.asTerm)(Symbol.spliceOwner).asExprOf[A]
    s"result: ${r.show}"
    r
 }
}
