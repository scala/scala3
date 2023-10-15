class CC
type Cap = CC^

def test(cap1: Cap, cap2: Cap) =
  var a: String ->{cap[test]} String = ???
  var b: List[String ->{cap[test]} String] = Nil
  def f(x: String): String = if cap1 == cap1 then "" else "a"
  a = f // ok
  val x = List(f)
  b = x  // ok
  b = List(f) // ok

  def scope(cap3: Cap) =
    def g(x: String): String = if cap3 == cap3 then "" else "a"
    a = (g: String => String)      // error
    a = g  // error
    b = List(g) // error

