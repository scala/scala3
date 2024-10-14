import language.experimental.captureChecking

trait Cap:
  def use: Int = 42

def test2(cs: List[Cap^]): Unit =
  val t0: Cap^{cs*} = cs.head  // error
  var t1: Cap^{cs*} = cs.head  // error
