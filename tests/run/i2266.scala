object Test extends App {
  def f = {
    lazy val x: true = { println("X"); true }
    println(x)
  }
  f
}

