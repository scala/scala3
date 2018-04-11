object App {
  trait A { type L >: Any}
  def upcast(a: A, x: Any): a.L = x
  lazy val p: A { type L <: Nothing } = p
  val q = new A { type L = Any }
  def coerce(x: Any): Int = upcast(p, x)  // error: not a legal path

  def compare(x: A, y: x.L) = assert(x == y)
  def compare2(x: A)(y: x.type) = assert(x == y)


  def main(args: Array[String]): Unit = {
    println(coerce("Uh oh!"))
    compare(p, p) // error: not a legal path
    compare2(p)(p) // error: not a legal path
  }
}
