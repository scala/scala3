

import scala.annotation.implicitNotFound

@implicitNotFound(msg = "Cannot construct a collection of type ${Too} with elements of type ${Elem} based on a collection of type ${From}.") // warn // warn
trait Meh[-From, +To]

@implicitNotFound(msg = "Cannot construct a collection of type ${To} ${Elem}.") // warn
trait Meh2[-From, +To]

class C[T](implicit @implicitNotFound("No C[${t}] available") t: T) // warn

trait T {
  def m[Aaa](implicit @implicitNotFound("I see no C[${Uuh}]") theC: C[Aaa]) = ??? // warn
  def n[Aaa](implicit @implicitNotFound("I see no C[${Aaa}]") theC: C[Aaa]) = ???
}

trait U[X, Y[_], Z[_, ZZ]] {
  class I[R] {
    def m[S](implicit @implicitNotFound("${X} ${Y} ${ Z } ${R} ${S} -- ${XX} ${ZZ} ${ Nix }") i: Int) = ??? // warn // warn // warn
  }
}