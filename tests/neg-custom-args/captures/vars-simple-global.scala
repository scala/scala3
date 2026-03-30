class CC
type Cap = CC^

val cap1: Cap = CC()
val cap2: Cap = CC()

def test() =
  var a: String ->{cap1, cap2} String = ???
  var b: List[String ->{cap1, cap2} String] = Nil
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

