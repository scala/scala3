import language.experimental.captureChecking
import caps.use

trait Cap:
  def use: Int = 42

def test2[C^](cs: List[Cap^{C}]): Unit =
  val t0: Cap^{C} = cs.head
  var t1: Cap^{C} = cs.head
