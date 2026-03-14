import util.boundary
import util.Try

def f(x: Int) =
  val t = boundary:
    Try: // error // error
      if x > 0 then
        boundary.break(Try(-x))
      x + 1
  t.get

def g(x: Int) =
  val t = Try:
    boundary:
      if x > 0 then
        boundary.break(-x)
      x + 1
  t.get
