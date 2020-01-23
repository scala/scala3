object Test {
  var firstArgName = ""
  var firstArgValue: Any = ""
  def main(args: Array[String]): Unit = {
    val x = new Foo("something", 2L, false)
    assert("p1" == firstArgName)
    assert("something" == firstArgValue)
  }
  def debug with foo.FirstArg : Unit = {
    firstArgName = summon[foo.FirstArg].source
    firstArgValue = summon[foo.FirstArg].value
  }
  class Foo(p1: String, p2: Long, p3: Boolean) {
    debug
  }
}
