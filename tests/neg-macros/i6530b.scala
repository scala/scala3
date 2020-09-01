import scala.quoted._
object Foo {
  def program(using QuoteContext) = '{
    val tpe: quoted.Type[Int] = ???
    val expr: quoted.Expr[Int] = ???

    val a: quoted.Expr[Int] = ${ Type[Int] } // error
    val b: quoted.Expr[Int] = '{ $tpe } // error
    val c: ${ '{ 43 } } = ???  // error
    val d: quoted.Type[Int] = Type[ $expr ] // error
  }
}
