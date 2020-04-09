sealed trait Command {
  type Reply
}

final case class Create() extends Command {
  type Reply = CreateReply
  val reply: Reply = CreateReply()
}

final case class Set() extends Command {
  type Reply = SetReply
  val reply: Reply = SetReply()
}

case class CreateReply()
case class SetReply()

def process[R](command: Command { type Reply = R }): R =
  command match {
    case create: Create => create.reply
    case set: Set       => set.reply
//                         ^
// Warning: unreachable code
  }
