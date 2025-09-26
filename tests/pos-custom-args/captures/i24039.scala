import language.experimental.captureChecking
import caps.*
def test1() =
  val z: Any^ = ???
  def onlyWithZ[C^](using c: Contains[C, z.type]) = ???
  onlyWithZ[{z}]     // error, but should be ok
def test2(z: Any^) =
  def onlyWithZ[C^](using c: Contains[C, z.type]) = ???
  onlyWithZ[{z}]     // ok
