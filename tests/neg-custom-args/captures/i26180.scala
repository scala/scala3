import scala.language.experimental.captureChecking

class IO

object Obj:
  type C^ = {caps.any}
  def bar(a: IO^{C}): IO^{C} = a

object Abs:
  type C^ <: {caps.any}
  def bar(a: IO^{C}): IO^{C} = a

def test(io: IO^): Unit =
  val x = Obj.bar(io) // error: local capability io cannot flow into the fixed set Obj.C
  val y = Abs.bar(io) // error: same for an abstract capture-set member
