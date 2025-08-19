import language.experimental.captureChecking
import caps.*
trait IRef:
  def get: Int
class Ref extends IRef, Mutable:
  private var _data: Int = 0
  def get: Int = _data
  update def set(x: Int): Unit = _data = x
def test1(a: Ref^{cap.rd}): Unit =
  a.set(42)  // error
def test2(a: Ref^{cap.rd}): Unit =
  val t: Ref^{cap} = a  // error
  def b: Ref^{cap.rd} = ???
  val i: IRef^{cap} = b // ok, no added privilege from `cap` on an IRef
  t.set(42)