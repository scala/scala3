import language.experimental.erasedDefinitions

import scala.annotation.implicitNotFound

sealed trait State
final class On extends State
final class Off extends State

@implicitNotFound("State must be Off")
class IsOff[S <: State]
object IsOff:
  inline given IsOff[Off]()

@implicitNotFound("State must be On")
class IsOn[S <: State]
object IsOn:
  inline given IsOn[On]()

class Machine[S <: State]:
  def turnOn(using erased IsOff[S]): Machine[On] =
    println("turnedOn")
    new Machine[On]

  def turnOff (using erased IsOn[S]): Machine[Off] =
    println("turnedOff")
    new Machine[Off]


@main def Test =
  val m = Machine[Off]()
  val m1 = m.turnOn
  val m2 = m1.turnOff
  m2.turnOn

  // m1.turnOn
  //          ^ error: State must be Off
  // m2.turnOff
  //           ^ error: State must be On
