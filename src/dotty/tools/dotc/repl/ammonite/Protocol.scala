package dotty.tools
package dotc
package repl
package ammonite.terminal

case class TermInfo(ts: TermState, width: Int)

sealed trait TermAction
case class Printing(ts: TermState, stdout: String) extends TermAction
case class TermState(
  inputs: LazyList[Int],
  buffer: Vector[Char],
  cursor: Int,
  msg: Ansi.Str = ""
) extends TermAction

object TermState {
  def unapply(ti: TermInfo): Option[(LazyList[Int], Vector[Char], Int, Ansi.Str)] =
    TermState.unapply(ti.ts)

  def unapply(ti: TermAction): Option[(LazyList[Int], Vector[Char], Int, Ansi.Str)] =
    ti match {
      case ts: TermState => TermState.unapply(ts)
      case _ => None
    }
}

case class ClearScreen(ts: TermState) extends TermAction
case object Exit extends TermAction
case class Result(s: String) extends TermAction
