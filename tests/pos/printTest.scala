// tests printing functionality

class C[X](x: Int, val y: String) {

}

object test extends C[String](1, "") {

  new C[Int](1, "") {}

  def foo(x: C[Int]) = new C[String](1, "") {}

}
