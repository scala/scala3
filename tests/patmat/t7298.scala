sealed trait Bool

object Bool {
  case object FALSE extends Bool
  case object TRUE extends Bool

  def show(b: Bool) = b match {
    case FALSE => "1"
    case TRUE  => "2"
  }
}
