import language.experimental.captureChecking
import caps.*
trait IRef:
  def get: Int
class Ref extends IRef, Mutable:
  private var _data: Int = 0
  def get: Int = _data
  update def set(x: Int): Unit = _data = x
def test1(a: Ref^{any.rd}): Unit =
  a.set(42)  // error
def test2(a: Ref^{any.rd}): Unit =
  val t: Ref^{any} = a  // error // error separation
  def b: Ref^{any.rd} = ???
  val i: IRef^{any} = b // ok, no added privilege from `any` on an IRef
  t.set(42)