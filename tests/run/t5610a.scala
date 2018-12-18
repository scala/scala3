object Test extends dotty.runtime.LegacyApp {
  class Result(_str: => String|Null) {
    lazy val str = _str
  }

  def foo(str: => String|Null)(i: Int) = new Result(str)

  def bar(f: Int => Result) = f(42)

  var test: String|Null = null
  val result = bar(foo(test))
  test = "bar"

  if (result.str == null) {
    println("Destroy ALL THE THINGS!!!")
  } else {
    println("Stroke a kitten")
  }
}
