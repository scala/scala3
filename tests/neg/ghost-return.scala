object Test {
  var b = true
  def foo(ghost a: Int): Int = {
    if (b)
      return a // error
    else
      return {
        println()
        a // error
      }
    42
  }
}
