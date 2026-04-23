trait T1
trait T2
trait T3
class Test

trait A[T]:
    this: T1 => 

trait D extends A[Int] // error: self type of D does not conform to that of A
trait E extends D

trait B[T]:
    this: T2 & T1 =>

trait F extends A[Int], B[Int] // error: self type of F does not conform to that of A

trait C[T]:
    this: T =>

trait H extends C[Test]: // error self type of H does not conform to that of C
    this: T3 =>

class Cl2 extends Test with H with T3
