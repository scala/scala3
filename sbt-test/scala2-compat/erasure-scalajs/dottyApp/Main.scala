object Main {
  def main(args: Array[String]): Unit = {
    val a = new scala2Lib.A
    assert(a.foo(1) == "1")
    assert(a.foo("") == "1")
    assert(a.foo(Array(1)) == "2")

    val b = new scala2Lib.B
    assert(b.foo(1) == "1")
    assert(b.foo("") == "1")
    assert(b.foo(Array(1)) == "2")
  }
}
