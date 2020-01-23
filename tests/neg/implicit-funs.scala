trait IF1 extends (Int ?=> Unit) // error
abstract class IF2 extends ((Int, String) ?=> Unit) // error
class IF3 extends (ImplicitFunction3[Int, String, Boolean, Unit]) { // error
  def apply with (Int, String, Boolean) = ()
}

trait IFXXL extends ((given // error
  Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int) => Unit)

val IFOK: (given  // OK
  Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int) => Unit = ()