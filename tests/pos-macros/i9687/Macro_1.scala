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

 def transformImpl[A](using Scope)(x: scope.Expr[A])(using scope.Type[A]): scope.Expr[A] = {
    import scope.tasty._
    val slowPath = '{ SlowPath }
    val fastPath = '{ FastPath }
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
    val r = transformer.transformTerm(x).seal.cast[A]
    s"result: ${r.show}"
    r
 }
}
