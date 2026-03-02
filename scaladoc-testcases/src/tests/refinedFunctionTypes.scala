package tests
package refinedFunctionTypes

import annotation.experimental

@experimental
infix type $throws[R, +E <: Exception] = CanThrow[E] ?=> R

@experimental
infix type $throws2[+E <: Exception] = (c: CanThrow[E]) ?=> c.type

@experimental
infix type $throws3[+E <: Exception] = [T] => (c: CanThrow[E]) ?=> c.type

@experimental
infix type $throws4[+E <: Exception] = [T] => (c: CanThrow[E]) ?=> T //expected: infix type $throws4[+E <: Exception] = [T] => CanThrow[E] ?=> T

type TA1 = (a: Int, b: (Boolean, String)) => List[(a.type, b.type)]

type TA2 = (a: Int, b: (Boolean, String)) ?=> List[Boolean]

@experimental
type TB0 = [R, E <: Exception] =>> PolyFunction { def apply[T](c: CanThrow[E]): R; } //expected: type TB0[R, E <: Exception] = [T] => CanThrow[E] => R

@experimental
type TB1 = [R, E <: Exception] =>> PolyFunction { def apply[T](c: CanThrow[E], y: c.type): R; } //expected: type TB1[R, E <: Exception] = [T] => (c: CanThrow[E], y: c.type) => R

@experimental
type TB2 = [R, E <: Exception] =>> PolyFunction { def apply[T](using c: CanThrow[E]): c.type; } //expected: type TB2[R, E <: Exception] = [T] => (c: CanThrow[E]) ?=> c.type

type TC1 = [T] => (a: T) => T //expected: type TC1 = [T] => T => T

type TC2 = [T] => (a: T) ?=> T //expected: type TC2 = [T] => T ?=> T

type TC3 = [T] => (a: T) => a.type

type TC4 = [T] => (a: T) ?=> a.type
