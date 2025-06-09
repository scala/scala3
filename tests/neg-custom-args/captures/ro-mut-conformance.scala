import language.experimental.captureChecking
import caps.*
class Ref extends Mutable:
  private var _data: Int = 0
  def get: Int = _data
  mut def set(x: Int): Unit = _data = x
def test1(a: Ref^{cap.rd}): Unit =
  a.set(42)  // error
def test2(a: Ref^{cap.rd}): Unit =
  val t: Ref^{cap} = a  // error
  t.set(42)