import scala.deriving.Mirror

type TC[A]

object TC:
  inline given derived[A](using Mirror.Of[A]): TC[A] = ???

abstract class Payload[A <: Payload[A]: TC]()

// infers Nothing and fail compilation
case class Summary() extends Payload() // error
