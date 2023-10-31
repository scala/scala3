
trait X { self: Y => } // error: missing requirement: self type Y & X of trait X does not conform to self type Z of required trait Y
trait Y { self: Z => }
trait Z

package squants:
  trait Quantity[A <: Quantity[A]] { self: A => }
  trait TimeDerivative[A <: Quantity[A]] { self: Quantity[?] => }