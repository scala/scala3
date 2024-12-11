trait A
trait B extends A
given b: B = ???
given a: A = ???

@annotation.nowarn("id=205")
val y = summon[A] // don't warn