trait C[+T <: C[T, U], -U <: C[T, U]] {

}
trait HasY { type Y }

// This works in scalac.
trait Foo1[-X] { def bar[Y <: X](y: Y) = y }

// A variant of Foo1 using a dependent method type (doesn't work using
// scalac)
trait Foo2[-X] { def bar(x: HasY { type Y <: X })(y: x.Y) = y }

// This works in scalac.
trait Foo3[+X] { def bar[Y >: X](y: Y) = y }

// A variant of Foo3 using a dependent method type (doesn't work
// using scalac)
trait Foo4[+X] { def bar(x: HasY { type Y >: X })(y: x.Y) = y }
