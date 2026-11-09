//> using options -language:experimental.specializedTraits

trait T1
trait T2
trait T3
class Test

inline trait A[T: Specialized]:
    this: T1 => 

inline trait D extends A[Int]:
    this: T1 =>
inline trait E extends D:
    this: T1 =>

inline trait B[T: Specialized]:
    this: T2 & T1 =>

inline trait F extends A[Int], B[Int]:
    this: T2 & T1 => 

inline trait C[T: Specialized]:
    this: T =>

inline trait H extends C[Test]:
    this: T3 & Test  =>

class Cl2 extends Test with H with T3
