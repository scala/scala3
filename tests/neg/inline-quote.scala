import scala.quoted._

object Test {

  inline def foo(x: Expr[Int]) given QuoteContext: Expr[Int] = '{ // error
    println("foo")
    ${
      ${??? : Expr[Int]}

      x
    }
  }


}
