object Test extends App {
  println(hello("Bill"))
  println(O.hi)
  println(hello(her))

  val x: Labelled[Int] = ("total", 3)
  println(showLabelled(x))
}