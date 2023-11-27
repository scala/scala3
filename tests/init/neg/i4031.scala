class App {
  trait A { type L >: Any}
  def upcast(a: A, x: Any): a.L = x
  val p: A { type L <: Nothing } = p        // error
  def coerce(x: Any): Int = upcast(p, x)

  def main(args: Array[String]): Unit = {
    println(coerce("Uh oh!"))
  }
}
