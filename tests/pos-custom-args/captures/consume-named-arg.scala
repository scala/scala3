import language.experimental.separationChecking

class Cap extends caps.ExclusiveCapability:
  def use(): Unit = ()

class Wrapper(val c: Cap)

def takeConsume(consume x: Wrapper^): Unit = ()

def test(consume c: Cap): Unit =
  val w: Wrapper^ = Wrapper(c)
  takeConsume(x = w)
