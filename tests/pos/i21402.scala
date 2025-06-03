abstract class AbstractServiceKey:
  type Protocol

abstract class ServiceKey[T] extends AbstractServiceKey:
  type Protocol = T

type Aux[P] = AbstractServiceKey { type Protocol = P }
type Service[K <: Aux[?]] = K match
  case Aux[t] => ActorRef[t]
type Subscriber[K <: Aux[?]] = K match
  case Aux[t] => ActorRef[ReceptionistMessages.Listing[t]]

trait ActorRef[-T]

object ReceptionistMessages:
  final case class Listing[T](key: ServiceKey[T])

class TypedMultiMap[T <: AnyRef, K[_ <: T]]:
  def get(key: T): Set[K[key.type]] = ???
  transparent inline def getInlined(key: T): Set[K[key.type]] = ???
  inline def inserted(key: T, value: K[key.type]): TypedMultiMap[T, K] = ???

object LocalReceptionist {
  final case class State(
      services: TypedMultiMap[AbstractServiceKey, Service],
      subscriptions: TypedMultiMap[AbstractServiceKey, Subscriber]
  ):
    def testInsert(key: AbstractServiceKey)(serviceInstance: ActorRef[key.Protocol]): State = {
      val fails = services.inserted(key, serviceInstance) // error
      ???
    }

  def testGet[T](key: AbstractServiceKey): Unit = {
    val newState: State = ???
    val fails: Set[ActorRef[key.Protocol]] = newState.services.get(key) // error
    val works: Set[ActorRef[key.Protocol]] = newState.services.getInlined(key) // workaround

    val fails2: Set[ActorRef[ReceptionistMessages.Listing[key.Protocol]]] = newState.subscriptions.get(key) // error
    val works2: Set[ActorRef[ReceptionistMessages.Listing[key.Protocol]]] = newState.subscriptions.getInlined(key) // workaround
  }
}
