trait A
class B extends A

inline given tc <: A = B()

val x: B = summon[A]

