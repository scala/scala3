class Box[T]
def dep1[T1 <: Singleton, T2 <: T1](t1: T1)(t2: T2): Box[T1] = ???
val d1 = dep1(1)(2)
