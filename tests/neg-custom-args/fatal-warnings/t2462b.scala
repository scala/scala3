package test

import scala.annotation.implicitNotFound

@implicitNotFound(msg = "Cannot construct a collection of type ${Too} with elements of type ${Elem} based on a collection of type ${From}.")
trait Meh[-From, +To]

@implicitNotFound(msg = "Cannot construct a collection of type ${To} ${Elem}.")
trait Meh2[-From, +To]

class C[T]
trait T {
  def m[Aaa](implicit theC: C[Aaa] @implicitNotFound("I see no C[${Uuh}]")) = ???
  def n[Aaa](implicit theC: C[Aaa] @implicitNotFound("I see no C[${Aaa}]")) = ???
}

trait U[X, Y[_], Z[_, ZZ]] {
  class I[R] {
    def m[S](implicit i: Int @implicitNotFound("${X} ${Y} ${ Z } ${R} ${S} -- ${XX} ${ZZ} ${ Nix }")) = ???
  }
}

// error
// error
// error