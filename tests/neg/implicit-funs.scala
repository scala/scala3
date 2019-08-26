trait IF1 extends (given Int => Unit) // error
abstract class IF2 extends (given (Int, String) => Unit) // error
class IF3 extends (ImplicitFunction3[Int, String, Boolean, Unit]) { // error
  def apply given (Int, String, Boolean) = ()
}

trait IFXXL extends (given ( // error
  Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int) => Unit)

val IFOK: given ( // OK
  Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int) => Unit = ()