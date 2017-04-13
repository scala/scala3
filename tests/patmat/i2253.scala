sealed trait S
object O extends S
trait T

class Test {
  def m(s: S { val x: Int }) = s match { case _: T => ; }
}