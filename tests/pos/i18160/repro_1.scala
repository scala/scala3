object core {
  final class CreationTicket[State[_]]
}

trait ReadAs[S[_], +A] { type State[V] = S[V] }

trait EventCompatBundle {
  bundle: Operators =>

  trait EventCompat[+T] extends ReadAs[State, Option[T]] {
    selfType: Event[T] =>
    final inline def map[B](inline expression: T => B)(implicit ticket: CreationTicket): Event[B] =  ???
  }
}

trait EventBundle extends EventCompatBundle { self: Operators =>
  trait Event[+T] extends EventCompat[T]:
    final override type State[V] = self.State[V]
}
trait Operators extends EventBundle {
  type State[_]
  type CreationTicket = core.CreationTicket[State]
}
trait RescalaInterface extends Operators

