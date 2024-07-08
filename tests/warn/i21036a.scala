//> using options -source 3.5
trait A
trait B extends A
given b: B = ???
given a: A = ???

val y = summon[A] // warn