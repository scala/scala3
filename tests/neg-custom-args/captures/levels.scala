//> using options -source 3.4
// (to make sure we use the sealed policy)
class CC

def test1(cap1: CC^) =

  class Ref[T](init: T):
    private var v: T = init
    def setV(x: T): Unit = v = x
    def getV: T = v

def test2(cap1: CC^) =

  class Ref[T](init: T):
    private var v: T = init
    def setV(x: T): Unit = v = x
    def getV: T = v

  val _ = Ref[String => String]((x: String) => x) // error
  val r = Ref((x: String) => x)

  def scope(cap3: CC^) =
    def g(x: String): String = if cap3 == cap3 then "" else "a"
    r.setV(g) // error
  ()
