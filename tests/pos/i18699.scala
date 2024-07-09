import language.experimental.captureChecking
import caps.unboxed

trait Cap:
  def use: Int = 42

def test2(@unboxed cs: List[Cap^]): Unit =
  val t0: Cap^{cs*} = cs.head  // error
  var t1: Cap^{cs*} = cs.head  // error
