import language.experimental.captureChecking
import caps.use

trait Cap:
  def use: Int = 42

def test2(cs: List[Cap^] @use): Unit =
  val t0: Cap^{cs*} = cs.head  // error
  var t1: Cap^{cs*} = cs.head  // error
