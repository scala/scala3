//> using options -Werror

class `C$Z`:
  override def toString = "C$Z"

  def patvar[A](x: Option[A]) =
    x match
    case Some(`funky$thing` @ _) => true
    case _ => false
