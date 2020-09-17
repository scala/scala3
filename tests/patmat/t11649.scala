sealed trait Command { type Err }
final case class Kick() extends Command { type Err = String }
final case class Box() extends Command { type Err = Int }
def handle[E](cmd: Command {type Err = E}): Either[E, Unit] = cmd match {
  case Kick() => ???
  case Box() => ???
}
