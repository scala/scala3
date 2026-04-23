trait T1
inline trait A[T]:
    this: T1 => 

class B extends A[Int] // error: illegal inheritance: class B does not conform to self type
class C extends A, T1
