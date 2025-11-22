class Has[A]
trait X
trait Y
trait Z

abstract class Test {
  def x: Has[X] | (Has[Y] & Has[Z])
  def foo[T <: Has[_]](has: T): T = has
  foo(x)
}

// -------------------------------------------

trait ZLayer[-RIn, +E, +ROut <: Has[_]] {
  def >>>[E1 >: E, ROut2 <: Has[_]](that: ZLayer[ROut, E1, ROut2]): ZLayer[RIn, E1, ROut2]
  def ++[E1 >: E, RIn2, ROut1 >: ROut <: Has[_], ROut2 <: Has[_]](that: ZLayer[RIn2, E1, ROut2]): ZLayer[RIn with RIn2, E1, ROut1 with ROut2]
}
object ZLayer {
  type NoDeps[+E, +B <: Has[_]] = ZLayer[Any, E, B]
}

type ServiceA = Has[ServiceA.Service]
object ServiceA {
  trait Service
  val live: ZLayer.NoDeps[Nothing, ServiceA] = ???
}

type ServiceB = Has[ServiceB.Service]
object ServiceB {
  trait Service
  val live: ZLayer.NoDeps[Nothing, ServiceB] = ???
}

type ServiceC = Has[ServiceC.Service]
object ServiceC {
  trait Service
  val live: ZLayer.NoDeps[Nothing, ServiceC] = ???
}

type ServiceD = Has[ServiceD.Service]
object ServiceD {
  trait Service
  val live: ZLayer.NoDeps[ServiceC, ServiceD with ServiceC] = ???
}

val combined =
    ServiceA.live >>>
      (ServiceB.live ++ (ServiceC.live >>> ServiceD.live))

// -------------------------------------------

class Outer {
  class Elem
}

abstract class Test2 {
  val o1: Outer = ???
  val o2: Outer = ???
  val o3: Outer = ???

  val x: o1.Elem | (o2.Elem & o3.Elem)
  def foo[T <: Outer#Elem](has: T): T = ???
  foo(x)
}