import scala.quoted._
object Foo {
  def program(given QuoteContext) = '{
    val tpe: TypeTag[Int] = ???
    val expr: Expr[Int] = ???

    val a: Expr[Int] = ${ '[Int] } // error
    val b: Expr[Int] = '{ $tpe } // error
    val c: ${ '{ 43 } } = ???  // error
    val d: TypeTag[Int] = '[ $expr ] // error
  }
}
