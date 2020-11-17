package x

import scala.quoted._

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

 def transformImpl[A:Type](x:Expr[A])(using qctx: QuoteContext):Expr[A] = {
    import qctx.reflect._
    val slowPath = Term.of('{ SlowPath })
    val fastPath = Term.of('{ FastPath })
    val transformer = new TreeMap() {
      override def transformTerm(term:Term)(using ctx:Context):Term = {
        term match
          case Apply(sel@Select(o,m),args) =>
                if ( o.tpe =:= slowPath.tpe && m=="sum" )
                   Apply(Select.unique(fastPath,"sum"), args)
                else
                   super.transformTerm(term)
          case _ => super.transformTerm(term)
      }
    }
    val r = transformer.transformTerm(Term.of(x)).asExprOf[A]
    s"result: ${r.show}"
    r
 }
}
