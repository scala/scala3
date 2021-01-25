trait C:
  inline def x: Int
  inline val y: Int

def test: Unit =
  val c: C = ???
  c.x // error
  c.y // error
