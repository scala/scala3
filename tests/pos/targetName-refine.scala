import annotation.targetName
trait T with
  @targetName("f2") def f: Any
class C extends T with
  @targetName("f2") def f: Int = 1

val x: T { def f: Int } = C()

