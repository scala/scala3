object Test {
  var i: Int = 1
  def foo(ghost a: Int): Int = {
    i = a // error
    ghost def r = {
      i = a
      ()
    }
    42
  }
}
