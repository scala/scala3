trait A
class B extends A

inline given tc as _ <: A = B()

val x: B = summon[A]

