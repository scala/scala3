//> using options -explain

class C[T](x : T = null)

def f[A](x: A = null) = x
def g[A <: Int, B](i: A)(x: B = null, y: Int) = x.toString * (i + y)

object Test {
  def main(args: Array[String]): Unit = {
    new C[Int] // error
    f[Int]() // error
    g[Int, Int](42)(y = 27) // error
  }
}
