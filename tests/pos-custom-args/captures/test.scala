class C
type Cap = C^

type Proc = () => Unit

class Ref(p: () => Unit):
  private var x: () => Unit = p
  def set(x: () ->{cap[Ref]} Unit): Unit = this.x = x
  def get: () => Unit = x

def test(c: () => Unit) =
  val p: () => Unit = ???
  val r = Ref(p)
  val x = r.get
  r.set(x)

