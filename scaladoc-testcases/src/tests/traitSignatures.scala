package tests
package traitSignatures

trait A

trait B extends A

trait C(a: Int)

trait D(b: Double) extends C, A

trait E extends Any
