enum PingMessage[Response]:
  case Ping(from: String) extends PingMessage[String]

val pongBehavior: [O] => (Unit, PingMessage[O]) => (Unit, O) =
  [P] =>
    (state: Unit, msg: PingMessage[P]) =>
      msg match
        case PingMessage.Ping(from) => ((), s"Pong from $from")
