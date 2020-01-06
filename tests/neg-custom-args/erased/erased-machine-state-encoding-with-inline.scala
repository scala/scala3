import scala.compiletime._

sealed trait State
final class On extends State
final class Off extends State

class Machine[S <: State] {
  inline def turnOn() <: Machine[On] = inline erasedValue[S] match {
    case _: Off  => new Machine[On]
    case _: On   => error("Turning on an already turned on machine")
  }
  inline def turnOff() <: Machine[Off] = inline erasedValue[S] match {
    case _: On  => new Machine[Off]
    case _: Off   => error("Turning off an already turned off machine")
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
