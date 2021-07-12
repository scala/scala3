trait MyBase[A]{
  def foo: String
}

case class BothThing[L, R]() extends MyBase[L & R]:
  def foo: String = "blather"

trait Has[A]

trait Console
trait Clock

type ConsoleWithClock = Has[Console] with Has[Clock]

class Spec[R <: Has[_]]

object MySpec1 extends Spec[Has[Console] with Has[Clock]] // does not compile
object MySpec2 extends Spec[ConsoleWithClock] // okay
