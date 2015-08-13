object Test {
  def fun(a: Int => Unit) = a(2)
  def foo: Int = {
    var s = 1
    s = try {fun(s = _); 3} catch{ case ex: Throwable => s = 4; 5 }
    s
  }
  def main(args: Array[String]): Unit = foo
}
