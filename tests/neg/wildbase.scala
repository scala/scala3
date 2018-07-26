class A[T]

class B extends A[_] // error: type argument must be fully defined

class C extends A[_ >: Any <: Nothing]  // error: conflicting bounds // error: type argument must be fully defined
