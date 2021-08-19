def Choice = (_: Any) match { case Int => Long; case Long => Int } // ok

type Choice[A] = A match { case Int => Long ; case Long => Int } // error