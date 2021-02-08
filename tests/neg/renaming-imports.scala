object a:
  def * = 1
  def other = 2

def test =
  object foo:
    var x = 0
  import foo as f  // error
  f.x

  import a.`*`
  println(*)      // ok
  println(other)  // error
end test
