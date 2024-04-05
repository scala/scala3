//> using options -language:experimental.modularity -source future
object Test:

  def someInt = 1

  def f1[T <: Singleton](x: T): T = x
  f1(someInt) // error
  f1(if ??? then 1 else 2) // OK, but should be error
  f1(3 * 2)   // OK

  def f2[T](x: T)(using T is Singleton): T = x
  f2(someInt) // error
  f2(if ??? then 1 else 2)   // error
  f2(3 * 2) // OK

  def f3[T: Singleton](x: T): T = x
  f3(someInt) // error
  f3(if ??? then 1 else 2)   // error
  f3(3 * 2) // OK
  f3(6) // OK
