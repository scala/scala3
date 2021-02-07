inline def meth =
  val x1 = inline ("a": Any) match
    case _: String => "ok"
  val x2 = inline { "a": Any } match
    case _: String => "ok"
  inline s match
    case _: String => "ok"

inline def s = "a": Any

def test = meth
