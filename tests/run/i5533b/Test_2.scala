object Test {

  def main(args: Array[String]): Unit = {
    import scalatest._
    val x = "String"
    println(assert(f(x) == "String"))
  }

}
