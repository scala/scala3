import annotation.targetName
trait T:
  @targetName("f2") def f: Any
class C extends T:
  @targetName("f2") def f: Int = 1

val x: T { def f: Int } = C() // error

