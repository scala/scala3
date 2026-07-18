

class CC
type Cap = CC^

def test(cap1: Cap, cap2: Cap) =
  def f(x: String): String = if cap1 == cap1 then "" else "a"
  var x = f
  val y = x
  val z = () => if x("") == "" then "a" else "b"
  val _: () ->{} String = z  // error
  val zc: () ->{cap1, x.rd} String = z
  val z2 = () => { x = identity }
  val z2c: () -> Unit = z2 // error
  var a = f

  var b: List[String ->{cap1, cap2} String] = Nil
  val u = a
  a("")
  b.head

  def scope(cap3: Cap) =
    def g(x: String): String = if cap3 == cap3 then "" else "a"
    def h(): String = ""
    a = x => g(x)      // error
    a = g      // error

    b = List(g) // error
    val gc = g
    g

  val s = scope(new CC)
  val sc: String => String = scope(new CC)

  def local[T](op: CC^ -> T): T = op(CC())

  local { cap3 => // error
    def g(x: String): String = if cap3 == cap3 then "" else "a"
    g
  }

  class Ref:
    @caps.unsafe.untrackedCaptures var elem: String ->{cap1} String = null

  val r = Ref()
  r.elem = f
  val fc = r.elem