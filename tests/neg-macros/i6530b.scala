import scala.quoted._
object Foo {
  def program(using s: Scope) = '{ (s2: Scope) ?=>

    val tpe: s2.Type[Int] = ???
    val expr: s2.Expr[Int] = ???

    val a: s2.Expr[Int] = ${ '[Int] } // error
    val b: s2.Expr[Int] = '{ $tpe } // error
    val c: ${ '{ 43 } } = ???  // error
    val d: s2.Type[Int] = '[ $expr ] // error
  }
}
