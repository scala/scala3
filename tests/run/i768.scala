case class A(a: String*){
  val s = a.toString
}

object Test {
  def main(args: Array[String]) = {
    println(A("a", "bc").s)
  }
}
