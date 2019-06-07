import scala.annotation.implicitNotFound

sealed trait State
final class On extends State
final class Off extends State

@implicitNotFound("State is must be Off")
class IsOff[S <: State]
object IsOff {
  implicit def isOff: IsOff[Off] = {
    println("isOff")
    new IsOff[Off]
  }
}

@implicitNotFound("State is must be On")
class IsOn[S <: State]
object IsOn {
  implicit def isOn: IsOn[On] = {
    println("isOn")
    new IsOn[On]
  }
}

class Machine[S <: State] private {
  def turnedOn given erased (s: IsOff[S]): Machine[On] = {
    println("turnedOn")
    new Machine[On]
  }
  def turnedOff given erased (s: IsOn[S]): Machine[Off] = {
    println("turnedOff")
    new Machine[Off]
  }
}

object Machine {
  def newMachine(): Machine[Off] = {
    println("newMachine")
    new Machine[Off]
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    val m = Machine.newMachine()
    m.turnedOn
    m.turnedOn.turnedOff

    // m.turnedOff
    //            ^
    //            State is must be On

    // m.turnedOn.turnedOn
    //                    ^
    //                    State is must be Off
  }
}
