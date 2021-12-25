class CC
type Cap = {*} CC

def test(cap1: Cap, cap2: Cap) =
  def f(x: String): String = if cap1 == cap1 then "" else "a"
  var x = f
  val y = x
  val z = () => if x("") == "" then "a" else "b"
  val zc: {cap1} () -> String = z
  val z2 = () => { x = identity }
  val z2c: () -> Unit = z2  // error

  var a: String => String = f // error
  var b: List[String => String] = Nil // error

  def scope =
    val cap3: Cap = CC()
    def g(x: String): String = if cap3 == cap3 then "" else "a"
    a = g
    b = List(g)
    val gc = g
    g

  val s = scope
  val sc: String => String = scope

  def local[T](op: Cap -> T): T = op(CC())

  local { cap3 => // error
    def g(x: String): String = if cap3 == cap3 then "" else "a"
    g
  }

  class Ref:
    var elem: {cap1} String -> String = null

  val r = Ref()
  r.elem = f
  val fc = r.elem
