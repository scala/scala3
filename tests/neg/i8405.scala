class ActorRef
trait ActorEventBus {
  type Subscriber = ActorRef
}
trait ManagedActorClassification { this: ActorEventBus =>
  def unsubscribe(subscriber: Subscriber): Unit = ???
}
class ActorClassificationUnsubscriber(bus: ManagedActorClassification) {
  val actor = ???
  bus.unsubscribe(actor)  // error
}