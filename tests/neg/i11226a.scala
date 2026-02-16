trait ActorRef

trait ActorEventBus {
  type Subscriber = ActorRef
}

trait ManagedActorClassification { this: ActorEventBus =>
  def unsubscribe(subscriber: Subscriber): Unit
}

class Unsubscriber(bus: ManagedActorClassification) {
  def test(a: ActorRef): Unit = bus.unsubscribe(a) // error
}