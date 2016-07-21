sealed trait EthernetType

object EthernetType {
  final case object Gigabit extends EthernetType
  final case object FastEthernet extends EthernetType

  final def toInt(t: EthernetType) = t match {
    case Gigabit => 1
    case FastEthernet => 2
  }
}