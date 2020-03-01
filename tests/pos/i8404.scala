trait ActorRefScope
class ActorRef
case class DeathWatchNotification(actor: ActorRef)
object Main {
  val ref: ActorRef = ???
  ref match {
    case r: ActorRefScope => DeathWatchNotification(r)
    case _ =>
  }
}