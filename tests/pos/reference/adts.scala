package adts
object t1 with

  enum Option[+T] with
    case Some(x: T)
    case None

object t2 with

  enum Option[+T] with
    case Some(x: T) extends Option[T]
    case None       extends Option[Nothing]

enum Color(val rgb: Int) with
  case Red   extends Color(0xFF0000)
  case Green extends Color(0x00FF00)
  case Blue  extends Color(0x0000FF)
  case Mix(mix: Int) extends Color(mix)

object t3 with

  enum Option[+T] with
    case Some(x: T) extends Option[T]
    case None

    def isDefined: Boolean = this match
      case None => false
      case some => true

  object Option with
    def apply[T >: Null](x: T): Option[T] =
      if (x == null) None else Some(x)
