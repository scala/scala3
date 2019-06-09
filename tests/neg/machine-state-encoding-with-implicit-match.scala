import scala.annotation.implicitNotFound

sealed trait State
final class On extends State
final class Off extends State

@implicitNotFound("State must be Off")
class IsOff[S <: State]
object IsOff {
  implied isOff for IsOff[Off] = new IsOff[Off]
}

@implicitNotFound("State must be On")
class IsOn[S <: State]
object IsOn {
  implied isOn for IsOn[On] = new IsOn[On]
}

class Machine[S <: State] {
  inline def turnOn() given (s: IsOff[S]) <: Machine[On] = implicit match {
    case _: IsOff[Off]  => new Machine[On]
  }
  inline def turnOff() given (s: IsOn[S]) <: Machine[Off] = implicit match {
    case _: IsOn[On]    => new Machine[Off]
  }
}

object Machine {
  def newMachine(): Machine[Off] = {
    println("newMachine")
    new Machine[Off]
  }
}

object Test {
  val m = Machine.newMachine()
  m.turnOn()
  m.turnOn().turnOff()
  m.turnOn().turnOn() // error: Turning on an already turned on machine
}
