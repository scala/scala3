import scala.quoted._

class Test {

  val x: Int = 0

  '{ '(x + 1)  // error: wrong staging level

    '((y: Expr[Int]) => ~y )  // error: wrong staging level

  }

  '(x + 1)  // error: wrong staging level

  '((y: Expr[Int]) => ~y )  // error: wrong staging level

  def f[T](t: Type[T], x: Expr[T]) = '{
    val z2 = ~x   // OK
  }

}
