transparent inline def f: String =
  inline 10 match
    case _ =>
      inline Some["foo"]("foo") match
        case Some(x) => x

def test =
  inline val failMsg = f
