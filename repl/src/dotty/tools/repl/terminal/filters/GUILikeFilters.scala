package dotty.tools
package repl
package terminal
package filters

import terminal.FilterTools._
import terminal.LazyList.~:
import terminal.SpecialKeys._
import terminal.DelegateFilter
import terminal._

/**
 * Filters have hook into the various {Ctrl,Shift,Fn,Alt}x{Up,Down,Left,Right}
 * combination keys, and make them behave similarly as they would on a normal
 * GUI text editor: alt-{left, right} for word movement, hold-down-shift for
 * text selection, etc.
 */
object GUILikeFilters {
  case class SelectionFilter(indent: Int) extends DelegateFilter {
    def identifier = "SelectionFilter"
    var mark: Option[Int] = None

    def setMark(c: Int) = {
      Debug("setMark\t" + mark + "\t->\t" + c)
      if (mark == None) mark = Some(c)
    }

    def doIndent(
      b: Vector[Char],
      c: Int,
      rest: LazyList[Int],
      slicer: Vector[Char] => Int
    ) = {

      val markValue = mark.get
      val (chunks, chunkStarts, chunkIndex) = FilterTools.findChunks(b, c)
      val min = chunkStarts.lastIndexWhere(_ <= math.min(c, markValue))
      val max = chunkStarts.indexWhere(_ > math.max(c, markValue))
      val splitPoints = chunkStarts.slice(min, max)
      val frags = (0 +: splitPoints :+ 99999).sliding(2).zipWithIndex

      var firstOffset = 0
      val broken =
        for((Seq(l, r), i) <- frags) yield {
          val slice = b.slice(l, r)
          if (i == 0) slice
          else {
            val cut = slicer(slice)

            if (i == 1) firstOffset = cut

            if (cut < 0) slice.drop(-cut)
            else Vector.fill(cut)(' ') ++ slice
          }
        }
      val flattened = broken.flatten.toVector
      val deeperOffset = flattened.length - b.length

      val (newMark, newC) =
        if (mark.get > c) (mark.get + deeperOffset, c + firstOffset)
        else (mark.get + firstOffset, c + deeperOffset)

      mark = Some(newMark)
      TS(rest, flattened, newC)
    }

    def filter = Filter.merge(

      Case(ShiftUp) {(b, c, m) => setMark(c); BasicFilters.moveUp(b, c, m.width)},
      Case(ShiftDown) {(b, c, m) => setMark(c); BasicFilters.moveDown(b, c, m.width)},
      Case(ShiftRight) {(b, c, m) => setMark(c); (b, c + 1)},
      Case(ShiftLeft) {(b, c, m) => setMark(c); (b, c - 1)},
      Case(AltShiftUp) {(b, c, m) => setMark(c); BasicFilters.moveUp(b, c, m.width)},
      Case(AltShiftDown) {(b, c, m) => setMark(c); BasicFilters.moveDown(b, c, m.width)},
      Case(AltShiftRight) {(b, c, m) => setMark(c); wordRight(b, c)},
      Case(AltShiftLeft) {(b, c, m) => setMark(c); wordLeft(b, c)},
      Case(FnShiftRight) {(b, c, m) => setMark(c); BasicFilters.moveEnd(b, c, m.width)},
      Case(FnShiftLeft) {(b, c, m) => setMark(c); BasicFilters.moveStart(b, c, m.width)},
      Filter("fnOtherFilter") {
        case TS(27 ~: 91 ~: 90 ~: rest, b, c, _) if mark.isDefined =>
          doIndent(b, c, rest,
            slice => -math.min(slice.iterator.takeWhile(_ == ' ').size, indent)
          )

        case TS(9 ~: rest, b, c, _) if mark.isDefined => // Tab
          doIndent(b, c, rest,
            slice => indent
          )

        // Intercept every other character.
        case TS(char ~: inputs, buffer, cursor, _) if mark.isDefined =>
          // If it's a special command, just cancel the current selection.
          if (char.toChar.isControl &&
              char != 127 /*backspace*/ &&
              char != 13 /*enter*/ &&
              char != 10 /*enter*/) {
            mark = None
            TS(char ~: inputs, buffer, cursor)
          } else {
            // If it's a  printable character, delete the current
            // selection and write the printable character.
            val Seq(min, max) = Seq(mark.get, cursor).sorted
            mark = None
            val newBuffer = buffer.take(min) ++ buffer.drop(max)
            val newInputs =
              if (char == 127) inputs
              else char ~: inputs
            TS(newInputs, newBuffer, min)
          }
      }
    )
  }

  object SelectionFilter {
    def mangleBuffer(
      selectionFilter: SelectionFilter,
      string: Ansi.Str,
      cursor: Int,
      startColor: Ansi.Attr
    ) = {
      selectionFilter.mark match {
        case Some(mark) if mark != cursor =>
          val Seq(min, max) = Seq(cursor, mark).sorted
          val displayOffset = if (cursor < mark) 0 else -1
          val newStr = string.overlay(startColor, min, max)
          (newStr, displayOffset)
        case _ => (string, 0)
      }
    }
  }

  val fnFilter = Filter.merge(
    Case(FnUp)((b, c, m) => (b, c - 9999)),
    Case(FnDown)((b, c, m) => (b, c + 9999)),
    Case(FnRight)((b, c, m) => BasicFilters.moveEnd(b, c, m.width)),
    Case(FnLeft)((b, c, m) => BasicFilters.moveStart(b, c, m.width))
  )
  val altFilter = Filter.merge(
    Case(AltUp)    {(b, c, m) => BasicFilters.moveUp(b, c, m.width)},
    Case(AltDown)  {(b, c, m) => BasicFilters.moveDown(b, c, m.width)},
    Case(AltRight) {(b, c, m) => wordRight(b, c)},
    Case(AltLeft)  {(b, c, m) => wordLeft(b, c)}
  )

  val fnAltFilter = Filter.merge(
    Case(FnAltUp)    {(b, c, m) => (b, c)},
    Case(FnAltDown)  {(b, c, m) => (b, c)},
    Case(FnAltRight) {(b, c, m) => (b, c)},
    Case(FnAltLeft)  {(b, c, m) => (b, c)}
  )
  val fnAltShiftFilter = Filter.merge(
    Case(FnAltShiftRight) {(b, c, m) => (b, c)},
    Case(FnAltShiftLeft)  {(b, c, m) => (b, c)}
  )


  def consumeWord(b: Vector[Char], c: Int, delta: Int, offset: Int) = {
    var current = c
    while(b.isDefinedAt(current) && !b(current).isLetterOrDigit) current += delta
    while(b.isDefinedAt(current) && b(current).isLetterOrDigit) current += delta
    current + offset
  }

  // c -1 to move at least one character! Otherwise you get stuck at the start of
  // a word.
  def wordLeft(b: Vector[Char], c: Int) = b -> consumeWord(b, c - 1, -1, 1)
  def wordRight(b: Vector[Char], c: Int) = b -> consumeWord(b, c, 1, 0)
}
