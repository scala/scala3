def g(cond: Boolean) = if cond then true else false
def f(x: Any) =
  x match
    case s: String if g:
      s.length > 4
    => s
    case x => x.toString
def fAligned(x: Any) =
  x match
  case s: String if g:
    s.length > 4
  => s
  case x => x.toString
def fNormalized(x: Any) =
  x match
  case s: String if g(s.length > 4) => s
  case x => x.toString

@main def main =
  println:
    f("hello, world")
