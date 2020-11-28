import scala.annotation.implicitNotFound
import scala.compiletime.summonFrom

sealed trait State
final class On extends State
final class Off extends State

@implicitNotFound("State must be Off")
class IsOff[S <: State]
object IsOff {
  given isOff: IsOff[Off] = new IsOff[Off]
}

@implicitNotFound("State must be On")
class IsOn[S <: State]
object IsOn {
  given isOn: IsOn[On] = new IsOn[On]
}

class Machine[S <: State] {
  transparent inline def turnOn()(using s: IsOff[S]): Machine[On] = summonFrom {
    case _: IsOff[Off]  => new Machine[On]
  }
  transparent inline def turnOff()(using s: IsOn[S]): Machine[Off] = summonFrom {
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
