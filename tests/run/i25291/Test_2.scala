object Test:
  def main(args: Array[String]): Unit =
    val app = new other.App
    // This would throw AbstractMethodError before the fix
    // because the bridge from Base.foo():Base to Middle.foo():Middle was missing
    val result = pkg.Helper.callFoo(app)
    println(s"result eq app: ${result eq app}")
    assert(result eq app, s"expected app instance but got $result")
