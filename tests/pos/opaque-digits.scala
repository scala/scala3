object pkg {

  import Character.{isAlphabetic, isDigit}

  class Alphabetic private[pkg] (val value: String) extends AnyVal

  object Alphabetic {
    def fromString(s: String): Option[Alphabetic] =
      if (s.forall(isAlphabetic(_))) Some(new Alphabetic(s))
      else None
  }

  opaque type Digits = String

  object Digits {
    def fromString(s: String): Option[Digits] =
      if (s.forall(isDigit(_))) Some(s)
      else None

    def asString(d: Digits): String = d
  }
}