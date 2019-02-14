object Test extends App {

  def (f: A => B) `$`[A, B](a: A): B = f(a)

  assert((((a: Int) => a.toString()) `$` 10) == "10")

  def g(x: Int): String = x.toString

  assert((g `$` 10) == "10")

  val h: Int => String = _.toString

  assert((h `$` 10) == "10")
}
