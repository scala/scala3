trait Transition[From, To]

infix type ==>[From, To] = Transition[From, To]

type A = A.type
object A

type B = B.type
object B

type C = C.type
object C

// Compiles
given (A ==> B) = ???

// was Compile error
given (A ==> C) = ???

given ==>[A, C] = ???  // error: double definition

given List[A ==> B] = ???
given List[A ==> C] = ???   // error: double definition
