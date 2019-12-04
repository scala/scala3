
object Test {
  def main(args: Array[String]) = {
    val c = new p.C()
    c.inl() // error (when inlined): not accessible
  }
}
