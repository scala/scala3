trait A {

  var x = 3
  println("hi")
  val y = x * x

  def f: Int = x + y

  def f(z: Int): Int = f + z

}
