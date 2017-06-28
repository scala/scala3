package dotty.tools
package repl
package terminal

/**
 * One place to assign all the esotic control key input snippets to
 * easy-to-remember names
 */
object SpecialKeys {

  /**
   * Lets you easily pattern match on characters modified by ctrl,
   * or convert a character into its ctrl-ed version
   */
  object Ctrl {
    def apply(c: Char) = (c - 96).toChar.toString
    def unapply(i: Int): Option[Int] = Some(i + 96)
  }

  /**
   * The string value you get when you hit the alt key
   */
  def Alt   = "\u001b"


  val Up    = Alt+"[A"
  val Down  = Alt+"[B"
  val Right = Alt+"[C"
  val Left  = Alt+"[D"

  val Home  = Alt+"OH"
  val End   = Alt+"OF"

  val Tab = 9.toChar.toString

  // For some reason Screen makes these print different incantations
  // from a normal snippet, so this causes issues like
  // https://github.com/lihaoyi/Ammonite/issues/152 unless we special
  // case them
  val HomeScreen     = Alt+"[1~"
  val EndScreen      = Alt+"[4~"
  val HomeLinuxXterm = Alt+"[7~"
  val EndRxvt        = Alt+"[8~"

  val ShiftUp        = Alt+"[1;2A"
  val ShiftDown      = Alt+"[1;2B"
  val ShiftRight     = Alt+"[1;2C"
  val ShiftLeft      = Alt+"[1;2D"

  val FnUp           = Alt+"[5~"
  val FnDown         = Alt+"[6~"
  val FnRight        = Alt+"[F"
  val FnLeft         = Alt+"[H"

  val AltUp          = Alt*2+"[A"
  val AltDown        = Alt*2+"[B"
  val AltRight       = Alt*2+"[C"
  val AltLeft        = Alt*2+"[D"

  val LinuxCtrlRight = Alt+"[1;5C"
  val LinuxCtrlLeft  = Alt+"[1;5D"

  val FnAltUp        = Alt*2+"[5~"
  val FnAltDown      = Alt*2+"[6~"
  val FnAltRight     = Alt+"[1;9F"
  val FnAltLeft      = Alt+"[1;9H"

  // Same as fn-alt-{up, down}
//  val FnShiftUp     = Alt*2+"[5~"
//  val FnShiftDown   = Alt*2+"[6~"
  val FnShiftRight  = Alt+"[1;2F"
  val FnShiftLeft   = Alt+"[1;2H"

  val AltShiftUp    = Alt+"[1;10A"
  val AltShiftDown  = Alt+"[1;10B"
  val AltShiftRight = Alt+"[1;10C"
  val AltShiftLeft  = Alt+"[1;10D"

  // Same as fn-alt-{up, down}
//  val FnAltShiftUp    = Alt*2+"[5~"
//  val FnAltShiftDown  = Alt*2+"[6~"
  val FnAltShiftRight = Alt+"[1;10F"
  val FnAltShiftLeft  = Alt+"[1;10H"
}
