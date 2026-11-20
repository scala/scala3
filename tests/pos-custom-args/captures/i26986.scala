//> using options -Ycc-log
import language.experimental.captureChecking

class Cap1 extends caps.ExclusiveCapability

def test(): Unit =
  val v1 = (x0: Cap1^) =>
    val u2 = new Cap1
    def d3(): Cap1^{u2} = u2
    d3()
    ()
