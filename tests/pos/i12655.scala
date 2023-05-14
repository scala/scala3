trait Signature:
  type Impl[A, U]

type Operation[Z <: Signature, A, U] = (z: Z) => z.Impl[A, U]

case class Perform0[Z <: Signature, A, U](op: Operation[Z, A, U])

case class Perform1[Z <: Signature, A, U](op: (z: Z) => z.Impl[A, U])

def perform0[Z <: Signature, A, U](op: Operation[Z, A, U]): Unit = ???

def perform1[Z <: Signature, A, U](op: (z: Z) => z.Impl[A, U]): Unit = ???
