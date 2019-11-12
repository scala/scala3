object Foo1 { type T[+A] = (A, Int) }
object Foo2 { type T[+A] = [+B] =>> (A, B) }
object Foo3 { type T[+A] = [+B] =>> [C] =>> (A, B) }
object Foo4 { type T = [+A] =>> [+B] =>> [C] =>> (A, B) }
