class A1[T](action: A1[T] ?=> String = "") // error
class A2[T](action: A1[T] ?=> String = summon[A1[T]]) // error
