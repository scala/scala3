import language.experimental.captureChecking
import caps.unbox

trait Cap:
  def use: Int = 42

def test2(@unbox cs: List[Cap^]): Unit =
  val t0: Cap^{cs*} = cs.head  // error
  var t1: Cap^{cs*} = cs.head  // error
