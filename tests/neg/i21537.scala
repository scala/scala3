// https://github.com/scala/scala3/issues/21537
trait Error
inline given Error  = compiletime.error("my error")
def foo(using Error): Int = 0
inline def bar: Int = compiletime.error("my error")
extension (arg: Int)
  def ~(arg2: Int)(using Error): Int = 0
  inline def !(arg2: Int): Int = compiletime.error("my error")

val x1 = 1 + (5 + foo) + 20 // error
val x2 = 1 + (5 ~ 200) + 20 // error
val x3 = 1 + (5 + bar) + 20 // error
val x4 = 1 + (5 ! 200) + 20 // error
