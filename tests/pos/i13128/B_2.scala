def grabT[T <: Int](arg : Foo[T]) : T = ???
final val t1 = grabT(??? : Foo[8])
val t2 : 8 = t1