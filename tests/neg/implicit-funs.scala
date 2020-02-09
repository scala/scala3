trait IF1 extends (Int ?=> Unit) // error
abstract class IF2 extends ((Int, String) ?=> Unit) // error
class IF3 extends (ContextFunction3[Int, String, Boolean, Unit]) { // error
  def apply(using Int, String, Boolean) = ()
}

trait IFXXL extends (( // error
  Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int) ?=> Unit)

val IFOK: (  // OK
  Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int) ?=> Unit = ()