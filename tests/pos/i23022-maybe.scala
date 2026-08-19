//> using options -Yexplicit-nulls
import language.experimental.magic
trait ExtractorWithImplicit:

  object Yikes:
    def unapply(implicit M: String): Any? = ???

  def expand: Any =
    given String = "Hey"
    "Wut" match
      case Yikes(_) => ???
      case _       => ???


