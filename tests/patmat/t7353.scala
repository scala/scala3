sealed trait EthernetType

object EthernetType {
  case object Gigabit extends EthernetType
  case object FastEthernet extends EthernetType

  final def toInt(t: EthernetType) = t match {
    case Gigabit => 1
    case FastEthernet => 2
  }
}