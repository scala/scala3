package dotty.tools.backend.sjs

final class ScopedVar[A <: AnyRef] private (init: A | Null, internal: Boolean) {
  import ScopedVar.Assignment

  private[ScopedVar] var value: A | Null = init

  def this(init: A) = this(init, true)
  def this() = this(null, true)

  def get: A =
    val v = value
    assert(v != null, "Trying to read a ScopedVar that is not set in the current scope")
    v

  def :=(newValue: A): Assignment[A] = new Assignment(this, newValue)

  def unset: Assignment[A] = new Assignment(this, null)
}

object ScopedVar {
  class Assignment[T <: AnyRef] private[ScopedVar] (scVar: ScopedVar[T], value: T | Null) {
    private[ScopedVar] def push(): AssignmentStackElement[T] = {
      val stack = new AssignmentStackElement(scVar, scVar.value)
      scVar.value = value
      stack
    }
  }

  private class AssignmentStackElement[T <: AnyRef](scVar: ScopedVar[T], oldValue: T | Null) {
    private[ScopedVar] def pop(): Unit = {
      scVar.value = oldValue
    }
  }

  implicit def toValue[T <: AnyRef](scVar: ScopedVar[T]): T = scVar.get

  def withScopedVars[T](ass: Assignment[?]*)(body: => T): T = {
    val stack = ass.map(_.push())
    try body
    finally stack.reverse.foreach(_.pop())
  }

  final class VarBox[A](var value: A)
}
