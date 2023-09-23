import annotation.retains
class C
type Cap = C @retains(caps.cap)

def test1() =
  val y: String^ = ""
  def x: Object @retains(y) = y

def test2() =
  val x: Cap = C()
  val y = () => { x; () }
  def z: (() -> Unit) @retains(x) = y
  z: (() -> Unit) @retains(x)
  def z2: (() -> Unit) @retains(y) = y
  z2: (() -> Unit) @retains(y)
  val p: () ->{cap[test2]} String = () => "abc"
  val q: C^{p} = ???
  val _ = p: (() ->{p} String)


