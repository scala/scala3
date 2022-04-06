object Test extends App {
  lazy val x: true = { println("X"); true }
  println(x)

  object Inner {
    println("Y")  // not printed
    inline val y = 1
  }
  println(Inner.y)

  inline val MV = Int.MaxValue
}

