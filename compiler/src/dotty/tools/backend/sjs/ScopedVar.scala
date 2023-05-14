package dotty.tools.backend.sjs

class ScopedVar[A](init: A) {
  import ScopedVar.Assignment

  private[ScopedVar] var value = init

  def this()(implicit ev: Null <:< A) = this(ev(null))

  def get: A = value
  def :=(newValue: A): Assignment[A] = new Assignment(this, newValue)
}

object ScopedVar {
  class Assignment[T](scVar: ScopedVar[T], value: T) {
    private[ScopedVar] def push(): AssignmentStackElement[T] = {
      val stack = new AssignmentStackElement(scVar, scVar.value)
      scVar.value = value
      stack
    }
  }

  private class AssignmentStackElement[T](scVar: ScopedVar[T], oldValue: T) {
    private[ScopedVar] def pop(): Unit = {
      scVar.value = oldValue
    }
  }

  implicit def toValue[T](scVar: ScopedVar[T]): T = scVar.get

  def withScopedVars[T](ass: Assignment[_]*)(body: => T): T = {
    val stack = ass.map(_.push())
    try body
    finally stack.reverse.foreach(_.pop())
  }

  final class VarBox[A](var value: A)
}
