
import scala.quoted._

class Foo {

  def let[T](s1: Sealed, s2: Sealed)(body: Expr[s1.Tpe] => Expr[T]): Expr[T] = '{
    val x: ~s2.tpe = ~s1.expr // error
    val y: ~s1.tpe = ~s2.expr // error
    val z: Any = ~s1.expr
    ~body('(x)) // error
    ~body('(y))
    ~body('(z)) // error
  }

}