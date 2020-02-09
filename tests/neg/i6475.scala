object Foo1 { type T[+A] = (A, Int) }
object Foo2 { type T[+A] = [+B] =>> (A, B) }   // error no `+/-` variance annotation allowed here
object Foo3 { type T[+A] = [+B] =>> [C] =>> (A, B) }  // error
object Foo4 { type T = [+A] =>> [+B] =>> [C] =>> (A, B) } // error // error
