case class A(a: String*){
  val s = a.toString
}

object Test {
  def main(args: Array[String]) = {
    assert(A("a", "bc").s == "ArraySeq(a, bc)")
  }
}


