trait A
class B extends A

transparent inline given tc as _: A = B()

val x: B = summon[A]

