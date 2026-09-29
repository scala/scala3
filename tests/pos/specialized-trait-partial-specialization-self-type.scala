//> using options -language:experimental.specializedTraits

trait T1
trait T2
trait T3
trait Test1
trait Test2

inline trait A[T: Specialized, S: Specialized, Z]:
    this: T1 & T & S => 

inline trait B[W: Specialized] extends A[W, Test2, Int]:
    this: T1 & W & Test2 =>

class Cl2 extends B[Test1] with Test1 with Test2 with T1

