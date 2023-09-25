//> using options -Werror
abstract class Namer:
  private enum CanForward:
    case Yes
    case No(whyNot: String)
    case Skip  // for members that have never forwarders

  class Mbr
  private def canForward(mbr: Mbr): CanForward = CanForward.Yes

  private def applyOrElse[A1 <: CanForward, B1 >: String](x: A1, default: A1 => B1): B1 = x match
    case CanForward.No(whyNot @ _) => whyNot
    case _ => ""

  def addForwardersNamed(mbrs: List[Mbr]) =
    val reason = mbrs.map(canForward).collect {
      case CanForward.No(whyNot) => whyNot
    }.headOption.getOrElse("")

  class ClassCompleter:
    def addForwardersNamed(mbrs: List[Mbr]) =
      val reason = mbrs.map(canForward).collect {
        case CanForward.No(whyNot) => whyNot
      }.headOption.getOrElse("")

    private def exportForwarders =
      def addForwardersNamed(mbrs: List[Mbr]) =
        val reason = mbrs.map(canForward).collect {
          case CanForward.No(whyNot) => whyNot
        }.headOption.getOrElse("")
        if mbrs.size == 4 then
          val reason = mbrs.map(canForward).collect {
            case CanForward.No(whyNot) => whyNot
          }.headOption.getOrElse("")
