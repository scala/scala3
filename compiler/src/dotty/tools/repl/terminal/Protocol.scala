package dotty.tools
package repl
package terminal

case class TermInfo(ts: TermState, width: Int)

trait TermAction
case class Printing(ts: TermState, stdout: String) extends TermAction
case class TermState(
  inputs: LazyList[Int],
  buffer: Vector[Char],
  cursor: Int,
  msg: Ansi.Str = Ansi.Str.parse("")
) extends TermAction

object TermState {
  // Using unapply instead exposes a dotty/scalac variation. Because
  // `TermState` is a case class, scalac generate an unapply with this exact
  // signature, that is used by the `TermInfo` and `TermAction` unapplies.
  // With dotty, the generated unapply has type `TermState => TermState`
  // instead, `unapply(ti: TermAction)` thus becomes a infinite tail
  // recursion. See #2335.
  def unapplyWorkaround(ti: TermState): Option[(LazyList[Int], Vector[Char], Int, Ansi.Str)] =
    Some((ti.inputs, ti.buffer, ti.cursor, ti.msg))

  def unapply(ti: TermInfo): Option[(LazyList[Int], Vector[Char], Int, Ansi.Str)] =
    TermState.unapplyWorkaround(ti.ts)

  def unapply(ti: TermAction): Option[(LazyList[Int], Vector[Char], Int, Ansi.Str)] =
    ti match {
      case ts: TermState => TermState.unapplyWorkaround(ts)
      case _ => None
    }
}

case class ClearScreen(ts: TermState) extends TermAction
case object Exit extends TermAction
case object Interrupt extends TermAction
case class Result(s: String) extends TermAction
