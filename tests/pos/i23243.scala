object Extractor:
  def unapply(s: String|Null): Boolean = true

class A

def main =
  ("foo": (A|String)) match
    case Extractor() => println("foo matched")
    case _ => println("foo didn't match")
