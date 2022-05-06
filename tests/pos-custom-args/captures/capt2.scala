import scala.retains
class C
type Cap = C @retains(*)

def test1() =
  val y: {*} String = ""
  def x: Object @retains(y) = y

def test2() =
  val x: Cap = C()
  val y = () => { x; () }
  def z: (() -> Unit) @retains(x) = y
  z: (() -> Unit) @retains(x)
  def z2: (() -> Unit) @retains(y) = y
  z2: (() -> Unit) @retains(y)
  val p: {*} () -> String = () => "abc"
  val q: {p} C = ???
  val _ = p: ({p} () -> String)


