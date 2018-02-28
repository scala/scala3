package dotty.tools
package repl
package terminal
package filters

import terminal.FilterTools._
import terminal.LazyList.~:
import terminal._
import scala.collection.mutable

/**
  * A filter that implements "undo" functionality in the ammonite REPL. It
  * shares the same `Ctrl -` hotkey that the bash undo, but shares behavior
  * with the undo behavior in desktop text editors:
  *
  * - Multiple `delete`s in a row get collapsed
  * - In addition to edits you can undo cursor movements: undo will bring your
  *   cursor back to location of previous edits before it undoes them
  * - Provides "redo" functionality under `Alt -`/`Esc -`: un-undo the things
  *   you didn't actually want to undo!
  *
  * @param maxUndo: the maximum number of undo-frames that are stored.
  */
case class UndoFilter(maxUndo: Int = 25) extends DelegateFilter("UndoFilter") {
  /**
    * The current stack of states that undo/redo would cycle through.
    *
    * Not really the appropriate data structure, since when it reaches
    * `maxUndo` in length we remove one element from the start whenever we
    * append one element to the end, which costs `O(n)`. On the other hand,
    * It also costs `O(n)` to maintain the buffer of previous states, and
    * so `n` is probably going to be pretty small anyway (tens?) so `O(n)`
    * is perfectly fine.
    */
  val undoBuffer = mutable.Buffer[(Vector[Char], Int)](Vector[Char]() -> 0)

  /**
    * The current position in the undoStack that the terminal is currently in.
    */
  var undoIndex = 0
  /**
    * An enum representing what the user is "currently" doing. Used to
    * collapse sequential actions into one undo step: e.g. 10 plai
    * chars typed becomes 1 undo step, or 10 chars deleted becomes one undo
    * step, but 4 chars typed followed by 3 chars deleted followed by 3 chars
    * typed gets grouped into 3 different undo steps
    */
  var state = UndoState.Default
  def currentUndo = undoBuffer(undoBuffer.length - undoIndex - 1)

  def undo(b: Vector[Char], c: Int) = {
    val msg =
      if (undoIndex >= undoBuffer.length - 1) UndoFilter.cannotUndoMsg
      else {
        undoIndex += 1
        state = UndoState.Default
        UndoFilter.undoMsg
      }
    val (b1, c1) = currentUndo
    (b1, c1, msg)
  }

  def redo(b: Vector[Char], c: Int) = {
    val msg =
      if (undoIndex <= 0) UndoFilter.cannotRedoMsg
      else {
        undoIndex -= 1
        state = UndoState.Default
        UndoFilter.redoMsg
      }

    currentUndo
    val (b1, c1) = currentUndo
    (b1, c1, msg)
  }

  def wrap(bc: (Vector[Char], Int, Ansi.Str), rest: LazyList[Int]) = {
    val (b, c, msg) = bc
    TS(rest, b, c, msg)
  }

  def pushUndos(b: Vector[Char], c: Int) = {
    val (lastB, lastC) = currentUndo
    // Since we don't have access to the `typingFilter` in this code, we
    // instead attempt to reverse-engineer "what happened" to the buffer by
    // comparing the old one with the new.
    //
    // It turns out that it's not that hard to identify the few cases we care
    // about, since they're all result in either 0 or 1 chars being different
    // between old and new buffers.
    val newState =
    // Nothing changed means nothing changed
      if (lastC == c && lastB == b) state
      // if cursor advanced 1, and buffer grew by 1 at the cursor, we're typing
      else if (lastC + 1 == c && lastB == b.patch(c-1, Nil, 1)) UndoState.Typing
      // cursor moved left 1, and buffer lost 1 char at that point, we're deleting
      else if (lastC - 1 == c && lastB.patch(c, Nil, 1) == b) UndoState.Deleting
      // cursor didn't move, and buffer lost 1 char at that point, we're also deleting
      else if (lastC == c && lastB.patch(c - 1, Nil, 1) == b) UndoState.Deleting
      // cursor moved around but buffer didn't change, we're navigating
      else if (lastC != c && lastB == b) UndoState.Navigating
      // otherwise, sit in the "Default" state where every change is recorded.
      else UndoState.Default

    if (state != newState || newState == UndoState.Default && (lastB, lastC) != (b, c)) {
      // If something changes: either we enter a new `UndoState`, or we're in
      // the `Default` undo state and the terminal buffer/cursor change, then
      // truncate the `undoStack` and add a new tuple to the stack that we can
      // build upon. This means that we lose all ability to re-do actions after
      // someone starts making edits, which is consistent with most other
      // editors
      state = newState
      undoBuffer.remove(undoBuffer.length - undoIndex, undoIndex)
      undoIndex = 0

      if (undoBuffer.length == maxUndo) undoBuffer.remove(0)

      undoBuffer.append(b -> c)
    } else if (undoIndex == 0 && (b, c) != undoBuffer(undoBuffer.length - 1)) {
      undoBuffer(undoBuffer.length - 1) = (b, c)
    }

    state = newState
  }

  def filter = Filter.merge(
    Filter.wrap("undoFilterWrapped") {
      case TS(q ~: rest, b, c, _) =>
        pushUndos(b, c)
        None
    },
    Filter.partial("undoFilter") {
      case TS(31 ~: rest, b, c, _) => wrap(undo(b, c), rest)
      case TS(27 ~: 114 ~: rest, b, c, _) => wrap(undo(b, c), rest)
      case TS(27 ~: 45 ~: rest, b, c, _) => wrap(redo(b, c), rest)
    }
  )
}


sealed class UndoState(override val toString: String)
object UndoState {
  val Default    = new UndoState("Default")
  val Typing     = new UndoState("Typing")
  val Deleting   = new UndoState("Deleting")
  val Navigating = new UndoState("Navigating")
}

object UndoFilter {
  val undoMsg       = Ansi.Color.Blue(Ansi.Str.parse(" ...undoing last action, `Alt -` or `Esc -` to redo"))
  val cannotUndoMsg = Ansi.Color.Blue(Ansi.Str.parse(" ...no more actions to undo"))
  val redoMsg       = Ansi.Color.Blue(Ansi.Str.parse(" ...redoing last action"))
  val cannotRedoMsg = Ansi.Color.Blue(Ansi.Str.parse(" ...no more actions to redo"))
}
