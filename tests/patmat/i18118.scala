//> using options -Werror

object O1:
  sealed trait A
  case class B() extends A
  case class C() extends A


  def bigMatch(x: A) = x match
    case B() =>
    case C() =>
    case _ => // error

object O2:
  sealed trait A
  case class B() extends A


  def bigMatch(x: A) = x match
    case B() =>
    case _ => // error // was: no "unreachable but for null" warning

object O3:
  sealed trait A
  case class B() extends A
  case class C() extends A


  def bigMatch(x: A) = x match
    case _: B =>
    case _: C =>
    case _ => // error

object O4:
  sealed trait A
  case class B() extends A


  def bigMatch(x: A) = x match
    case _: B =>
    case _ => // error
