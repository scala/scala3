trait A
class B extends A

transparent inline given tc: A = B()

val x: B = summon[A]

