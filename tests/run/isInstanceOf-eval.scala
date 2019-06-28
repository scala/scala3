object Test extends App {
  lazy val any = {
    println(1)
    1: Any
  }

  any.isInstanceOf[Int]

  lazy val int = {
    println(2)
    2
  }

  int.isInstanceOf[Int]
}
