import scala.quoted._
object Foo {
  def program(using QuoteContext) = '{
    val tpe: quoted.Staged[Int] = ???
    val expr: quoted.Expr[Int] = ???

    val a: quoted.Expr[Int] = ${ '[Int] } // error
    val b: quoted.Expr[Int] = '{ $tpe } // error
    val c: ${ '{ 43 } } = ???  // error
    val d: quoted.Staged[Int] = '[ $expr ] // error
  }
}
