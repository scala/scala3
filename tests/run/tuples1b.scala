object Test extends App {
  def x[T](xs: (T, T)): T = xs.head
  def x1[T](xs: (T, T)): Tuple1[T] = xs.tail
  def x2[T](xs: (T, T)): T = xs(0)
  def x3[T](xs: (T, T)): Unit = xs(0)

  println(x((0,1)))
  println(x1((0,1)))
  println(x2((0,1)))
  println(x3((0,1)))

  def println(x: Any): Unit =
    Console.println(if (x == ()) "()" else x) // portable on Scala.js
}
