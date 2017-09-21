class A[T]

class B extends A[_] // OK

class C extends A[_ >: Any <: Nothing]  // error: conflicting bounds
