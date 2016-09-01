package p {
class C {
  protected def f(): Unit = ()

  @dotty.annotation.inline
  def inl() = f() // error (when inlined): not accessible
}
}

object Test {
  def main(args: Array[String]) = {
    val c = new p.C()
    c.inl()
  }
}
