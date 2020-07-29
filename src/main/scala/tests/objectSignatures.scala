package tests
package objectSignatures

class A[T]

// TODO companion objects are not supported
// object A

trait C

object Base

object A2 extends A[String] with C

// We are not going to add final below
// final object B 

// We probably need to add a support for it
// private[tests] object D
