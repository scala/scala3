import scala.quoted._

object Test {

  inline def foo(using s: Scope)(x: s.Expr[Int]): s.Expr[Int] = '{ // error
    println("foo")
    ${
      ${??? : scope.Expr[Int]}

      x
    }
  }


}
