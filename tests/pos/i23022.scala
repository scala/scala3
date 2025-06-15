trait ExtractorWithImplicit:

  object Yikes:
    def unapply(implicit M: String): Option[Any] = ???

  def expand: Any =
    given String = "Hey"
    "Wut" match
      case Yikes(_) => ???
      case _       => ???


