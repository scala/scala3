import annotation.retains
class C
type Cap = C @retains[caps.any.type]

def test1() =
  val y: C^ = C()
  def x: Object @retains[y.type] = y

def test2() =
  val x: Cap = C()
  val y = () => { x; () }
  def z: (() -> Unit) @retains[x.type] = y
  z: (() -> Unit) @retains[x.type]
  def z2: (() -> Unit) @retains[y.type] = y
  z2: (() -> Unit) @retains[y.type]
  val p: () => String = () => "abc"
  val q: C^{p} = ???
  val _ = p: (() ->{p} String)


