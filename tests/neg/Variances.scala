trait HasY { type Y }

// These are neg-tests corresponding to the pos-test Variances.scala
// where all the variance annotations have been inverted.
trait Foo1[+X] { def bar[Y <: X](y: Y) = y } // error
trait Foo2[+X] { def bar(x: HasY { type Y <: X })(y: x.Y) = y } // error
trait Foo3[-X] { def bar[Y >: X](y: Y) = y } // error
trait Foo4[-X] { def bar(x: HasY { type Y >: X })(y: x.Y) = y } // error

// These are neg-tests corresponding to the pos-test Variances.scala
// where all the bounds have been flipped.
trait Foo5[-X] { def bar[Y >: X](y: Y) = y } // error
trait Foo6[-X] { def bar(x: HasY { type Y >: X })(y: x.Y) = y } // error
trait Foo7[+X] { def bar[Y <: X](y: Y) = y } // error
trait Foo8[+X] { def bar(x: HasY { type Y <: X })(y: x.Y) = y } // error
