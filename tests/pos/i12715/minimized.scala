transparent inline def f: String =
  inline 10 match
    case _ =>
      inline "foo" match
        case x : String => x

def test =
  inline val failMsg = f
