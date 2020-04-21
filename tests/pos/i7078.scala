trait A
class B extends A

transparent inline given tc as A = B()

val x: B = summon[A]

