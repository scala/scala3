val poly = [T] => (x: T) => x
val foo = new PolyFunction { } // error
val bar = new PolyFunction { def bar = 23 } // error
val baz = new PolyFunction { def apply = 23 } // error
val qux = new PolyFunction { def apply[T] = 47 } // error
val quxx = new PolyFunction { def apply[T](x: T): T = x } // error

trait PolyTrait extends PolyFunction // error

class PolyClass extends PolyTrait { // error
  def apply[T](x: T): T = x
}

object PolyObject extends PolyFunction // error
