object Test {
  def main(args: Array[String]): Unit = {
    assert(Macro.foo[Foo] == "symbol")
    assert(Macro.foo[Box[_]] == "no symbol")
  }
}
