package tests

package hierarchy

trait A1
trait A2[T]
trait A3[A, B]
trait A4

trait B1 extends A1
trait B2 extends A1 with A2[Int]
trait B3 extends A2[Int] with A3[Int, String]

class C1[A, B, C] extends B1 with B2 with B3

trait D1
trait D2[T, R]
trait D3 extends A4

class E1 extends C1[Int, String, Boolean] with D1
class E2 extends C1[Int, Boolean, Any] with D2[Int, Boolean] with D3
