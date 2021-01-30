object Test {

  def main(args: Array[String]): Unit = {
    import scalatest.*
    val x = "String"
    println(assert(f(x) == "String"))
  }

}
