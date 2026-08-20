type Unapply[A] = A match {
  case Unit => Boolean
  case _    => Option[A]
}

class Extractor[A] {
  def unapply(s: String): Unapply[A] = ???
}

@main
def main = {
  val extractor = Extractor[Unit]
  "" match {
    case extractor() =>
  }
}

