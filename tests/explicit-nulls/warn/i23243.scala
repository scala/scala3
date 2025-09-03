object Extractor:
  def unapply(s: String|Null): Boolean = true

class A

def main =
  ("foo": (A|String)) match
    case Extractor() => println("foo matched") // warn: String | Null is nullable
    case _ => println("foo didn't match")
  val s: Some[Any] = Some(5)

  s match {
    case Some(s: (String | Null)) => // warn: String | Null is nullable
    case Some(Some(s: (String | Null))) => // warn: String | Null is nullable
    case _: s.type =>
    case Some(s: Int) =>
    case _ =>
  }
