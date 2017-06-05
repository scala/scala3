package dotty.tools
package repl
package terminal
package filters

import terminal.FilterTools._
import terminal.SpecialKeys._
import terminal.{DelegateFilter, Filter, Terminal}
/**
 * Filters for injection of readline-specific hotkeys, the sort that
 * are available in bash, python and most other interactive command-lines
 */
object ReadlineFilters {
  // www.bigsmoke.us/readline/shortcuts
  // Ctrl-b     <- one char
  // Ctrl-f     -> one char
  // Alt-b      <- one word
  // Alt-f      -> one word
  // Ctrl-a     <- start of line
  // Ctrl-e     -> end of line
  // Ctrl-x-x   Toggle start/end

  // Backspace  <- delete char
  // Del        -> delete char
  // Ctrl-u     <- delete all
  // Ctrl-k     -> delete all
  // Alt-d      -> delete word
  // Ctrl-w     <- delete word

  // Ctrl-u/-   Undo
  // Ctrl-l     clear screen

  // Ctrl-k     -> cut all
  // Alt-d      -> cut word
  // Alt-Backspace  <- cut word
  // Ctrl-y     paste last cut

  /**
   * Basic readline-style navigation, using all the obscure alphabet hotkeys
   * rather than using arrows
   */
  lazy val navFilter = Filter.merge(
    Case(Ctrl('b'))((b, c, m) => (b, c - 1)), // <- one char
    Case(Ctrl('f'))((b, c, m) => (b, c + 1)), // -> one char
    Case(Alt + "b")((b, c, m) => GUILikeFilters.wordLeft(b, c)), // <- one word
    Case(Alt + "B")((b, c, m) => GUILikeFilters.wordLeft(b, c)), // <- one word
    Case(LinuxCtrlLeft)((b, c, m) => GUILikeFilters.wordLeft(b, c)), // <- one word
    Case(Alt + "f")((b, c, m) => GUILikeFilters.wordRight(b, c)), // -> one  word
    Case(Alt + "F")((b, c, m) => GUILikeFilters.wordRight(b, c)), // -> one  word
    Case(LinuxCtrlRight)((b, c, m) => GUILikeFilters.wordRight(b, c)), // -> one word
    Case(Home)((b, c, m) => BasicFilters.moveStart(b, c, m.width)), // <- one line
    Case(HomeScreen)((b, c, m) => BasicFilters.moveStart(b, c, m.width)), // <- one line
    Case(Ctrl('a'))((b, c, m) => BasicFilters.moveStart(b, c, m.width)),
    Case(End)((b, c, m) => BasicFilters.moveEnd(b, c, m.width)), // -> one line
    Case(EndScreen)((b, c, m) => BasicFilters.moveEnd(b, c, m.width)), // -> one line
    Case(Ctrl('e'))((b, c, m) => BasicFilters.moveEnd(b, c, m.width)),
    Case(Alt + "t")((b, c, m) => transposeWord(b, c)),
    Case(Alt + "T")((b, c, m) => transposeWord(b, c)),
    Case(Ctrl('t'))((b, c, m) => transposeLetter(b, c))
  )

  def transposeLetter(b: Vector[Char], c: Int) =
    // If there's no letter before the cursor to transpose, don't do anything
    if (c == 0) (b, c)
    else if (c == b.length) (b.dropRight(2) ++ b.takeRight(2).reverse, c)
    else (b.patch(c-1, b.slice(c-1, c+1).reverse, 2), c + 1)

  def transposeWord(b: Vector[Char], c: Int) = {
    val leftStart0 = GUILikeFilters.consumeWord(b, c - 1, -1, 1)
    val leftEnd0   = GUILikeFilters.consumeWord(b, leftStart0, 1, 0)
    val rightEnd   = GUILikeFilters.consumeWord(b, c, 1, 0)
    val rightStart = GUILikeFilters.consumeWord(b, rightEnd - 1, -1, 1)

    // If no word to the left to transpose, do nothing
    if (leftStart0 == 0 && rightStart == 0) (b, c)
    else {
      val (leftStart, leftEnd) =
        // If there is no word to the *right* to transpose,
        // transpose the two words to the left instead
        if (leftEnd0 == b.length && rightEnd == b.length) {
          val leftStart = GUILikeFilters.consumeWord(b, leftStart0 - 1, -1, 1)
          val leftEnd   = GUILikeFilters.consumeWord(b, leftStart, 1, 0)
          (leftStart, leftEnd)
        }else (leftStart0, leftEnd0)

      val newB =
        b.slice(0, leftStart)         ++
        b.slice(rightStart, rightEnd) ++
        b.slice(leftEnd, rightStart)  ++
        b.slice(leftStart, leftEnd)   ++
        b.slice(rightEnd, b.length)

      (newB, rightEnd)
    }
  }

  /**
   * All the cut-pasting logic, though for many people they simply
   * use these shortcuts for deleting and don't use paste much at all.
   */
  case class CutPasteFilter() extends DelegateFilter {
    def identifier = "CutPasteFilter"
    var accumulating = false
    var currentCut = Vector.empty[Char]
    def prepend(b: Vector[Char]) = {
      if (accumulating) currentCut = b ++ currentCut
      else currentCut = b
      accumulating = true
    }
    def append(b: Vector[Char]) = {
      if (accumulating) currentCut = currentCut ++ b
      else currentCut = b
      accumulating = true
    }
    def cutCharLeft(b: Vector[Char], c: Int) = {
      /* Do not edit current cut. Zsh(zle) & Bash(readline) do not edit the yank ring for Ctrl-h */
      (b patch(from = c - 1, patch = Nil, replaced = 1), c - 1)
    }

    def cutAllLeft(b: Vector[Char], c: Int) = {
      prepend(b.take(c))
      (b.drop(c), 0)
    }
    def cutAllRight(b: Vector[Char], c: Int) = {
      append(b.drop(c))
      (b.take(c), c)
    }

    def cutWordRight(b: Vector[Char], c: Int) = {
      val start = GUILikeFilters.consumeWord(b, c, 1, 0)
      append(b.slice(c, start))
      (b.take(c) ++ b.drop(start), c)
    }

    def cutWordLeft(b: Vector[Char], c: Int) = {
      val start = GUILikeFilters.consumeWord(b, c - 1, -1, 1)
      prepend(b.slice(start, c))
      (b.take(start) ++ b.drop(c), start)
    }

    def paste(b: Vector[Char], c: Int) = {
      accumulating = false
      (b.take(c) ++ currentCut ++ b.drop(c), c + currentCut.length)
    }

    def filter = Filter.merge(
      Case(Ctrl('u'))((b, c, m) => cutAllLeft(b, c)),
      Case(Ctrl('k'))((b, c, m) => cutAllRight(b, c)),
      Case(Alt + "d")((b, c, m) => cutWordRight(b, c)),
      Case(Ctrl('w'))((b, c, m) => cutWordLeft(b, c)),
      Case(Alt + "\u007f")((b, c, m) => cutWordLeft(b, c)),
      // weird hacks to make it run code every time without having to be the one
      // handling the input; ideally we'd change Filter to be something
      // other than a PartialFunction, but for now this will do.

      // If some command goes through that's not appending/prepending to the
      // kill ring, stop appending and allow the next kill to override it
      Filter.wrap("ReadLineFilterWrap") {_ => accumulating = false; None},
      Case(Ctrl('h'))((b, c, m) => cutCharLeft(b, c)),
      Case(Ctrl('y'))((b, c, m) => paste(b, c))
    )
  }
}
