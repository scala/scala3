class C
type Cap = C^

type Proc = () => Unit

class Ref[T](p: T) extends caps.Mutable:
  private var x: T = p
  update def set(x: T): Unit = this.x = x
  def get: T = x

def test(c: () => Unit) =
  val p: () => Unit = ???
  val r = Ref(p)
  val x = r.get
  r.set(x)

