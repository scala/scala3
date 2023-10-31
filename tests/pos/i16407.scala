trait X { //missing requirement: self type Z[?] & X of trait X does not conform to self type Z[X.this.A] of required trait Z
  self: Z[?] =>
}

trait Z[A] extends X {
  self: Z[A] => // comment this to compile successfully
}