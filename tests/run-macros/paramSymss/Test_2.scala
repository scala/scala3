
object Test {
  def a: Unit = ()
  def b(i: Int): Unit = ()
  def c(x: Int)(y: String)(z: Boolean): Unit = ()
  def d[T]: Unit = ()
  def c[T, U](x: Int, x2: Int)(y: String)(z: Boolean): Unit = ()

  def main(args: Array[String]) =
    println(showParamSyms(a))
    println(showParamSyms(b(1)))
    println(showParamSyms(c(2)("")(true)))
    println(showParamSyms(d[Int]))
    println(showParamSyms(c[Int, Char](3, 4)("")(true)))
    println(showParamSyms(new Test(1)))
    println(showParamSyms(new Test(1, 2)))
}

class Test[T](a: T):
    def this(a: Int, b: T) = this(b)
