trait A
class B extends A

transparent inline given A as tc = B()

val x: B = summon[A]

