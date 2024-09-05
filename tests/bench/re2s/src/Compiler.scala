// Copyright 2010 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Original Go source here:
// http://code.google.com/p/go/source/browse/src/pkg/regexp/syntax/compile.go

package java.util.regex

import java.util.regex.Compiler._
import java.util.regex.Inst.{Op => IOP}
import java.util.regex.Regexp.{Op => ROP}

/**
 * Compiler from {@code Regexp} (RE2 abstract syntax) to {@code RE2}
 * (compiled regular expression).
 *
 * The only entry point is {@link #compileRegexp}.
 */
class Compiler private () {
  private val prog: Prog = new Prog() // Program being built
  newInst(IOP.FAIL)

  private def newInst(op: IOP): Frag = {
    prog.addInst(op)
    new Frag(prog.numInst() - 1, 0)
  }

  // Returns a no-op fragment.  Sometimes unavoidable.
  private def nop(): Frag = {
    val f: Compiler.Frag = newInst(IOP.NOP)
    f.out = f.i << 1
    f
  }

  private def fail(): Compiler.Frag = new Frag(0, 0)

  // Given fragment a, returns (a) capturing as \n.
  // Given a fragment a, returns a fragment with capturing parens around a.
  private def cap(arg: Int): Frag = {
    val f: Compiler.Frag = newInst(IOP.CAPTURE)
    f.out = f.i << 1
    prog.getInst(f.i).arg = arg
    if (prog.numCap < arg + 1) {
      prog.numCap = arg + 1
    }
    f
  }

  // Given fragments a and b, returns ab a|b
  private def cat(f1: Frag, f2: Frag): Frag = {
    // concat of failure is failure
    if (f1.i == 0 || f2.i == 0) {
      return fail()
    }
    prog.patch(f1.out, f2.i)
    new Frag(f1.i, f2.out)
  }

  // Given fragments for a and b, returns fragment for a|b.
  private def alt(f1: Frag, f2: Frag): Frag = {
    // alt of failure is other
    if (f1.i == 0) {
      return f2
    }
    if (f2.i == 0) {
      return f1
    }
    val f: Compiler.Frag = newInst(IOP.ALT)
    val i: Inst = prog.getInst(f.i)
    i.out = f1.i
    i.arg = f2.i
    f.out = prog.append(f1.out, f2.out)
    f
  }

  // Given a fragment for a, returns a fragment for a? or a?? (if nongreedy)
  private def quest(f1: Frag, nongreedy: Boolean): Frag = {
    val f: Compiler.Frag = newInst(IOP.ALT)
    val i: Inst = prog.getInst(f.i)
    if (nongreedy) {
      i.arg = f1.i
      f.out = f.i << 1
    } else {
      i.out = f1.i
      f.out = f.i << 1 | 1
    }
    f.out = prog.append(f.out, f1.out)
    f
  }

  // Given a fragment a, returns a fragment for a* or a*? (if nongreedy)
  private def star(f1: Frag, nongreedy: Boolean): Frag = {
    val f: Compiler.Frag = newInst(IOP.ALT)
    val i: Inst = prog.getInst(f.i)
    if (nongreedy) {
      i.arg = f1.i
      f.out = f.i << 1
    } else {
      i.out = f1.i
      f.out = f.i << 1 | 1
    }
    prog.patch(f1.out, f.i)
    f
  }

  // Given a fragment for a, returns a fragment for a+ or a+? (if nongreedy)
  private def plus(f1: Frag, nongreedy: Boolean): Frag =
    new Frag(f1.i, star(f1, nongreedy).out)

  // op is a bitmask of EMPTY_* flags.
  private def empty(op: Int): Frag = {
    val f: Compiler.Frag = newInst(IOP.EMPTY_WIDTH)
    prog.getInst(f.i).arg = op
    f.out = f.i << 1
    f
  }

  // flags : parser flags
  private def rune(runes: Array[Int], _flags: Int): Frag = {
    var flags: Int = _flags
    val f: Compiler.Frag     = newInst(IOP.RUNE)
    val i: Inst     = prog.getInst(f.i)
    i.runes = runes
    flags &= RE2.FOLD_CASE // only relevant flag is FoldCase
    if (runes.length != 1 || Unicode.simpleFold(runes(0)) == runes(0)) {
      flags &= ~RE2.FOLD_CASE // and sometimes not even that
    }
    i.arg = flags
    f.out = f.i << 1
    // Special cases for exec machine.
    if ((flags & RE2.FOLD_CASE) == 0 && runes.length == 1 ||
        runes.length == 2 &&
        runes(0) == runes(1)) {
      i.op = IOP.RUNE1
    } else if (runes.length == 2 &&
               runes(0) == 0 &&
               runes(1) == Unicode.MAX_RUNE) {
      i.op = IOP.RUNE_ANY
    } else if (runes.length == 4 &&
               runes(0) == 0 &&
               runes(1) == '\n' - 1 &&
               runes(2) == '\n' + 1 &&
               runes(3) == Unicode.MAX_RUNE) {
      i.op = IOP.RUNE_ANY_NOT_NL
    }
    f
  }

  private def compile(re: Regexp): Frag = {
    re.op match {
      case ROP.NO_MATCH =>
        fail()
      case ROP.EMPTY_MATCH =>
        nop()
      case ROP.LITERAL =>
        if (re.runes.length == 0) {
          nop()
        } else {
          val runes: Array[Int]   = re.runes
          var f: Frag = null
          var i: Int       = 0
          while (i < runes.length) {
            val r: Int  = runes(i)
            val f1: Compiler.Frag = rune(Array[Int](r), re.flags)
            f = if (f == null) f1 else cat(f, f1)
            i += 1
          }
          f
        }
      case ROP.CHAR_CLASS =>
        rune(re.runes, re.flags)
      case ROP.ANY_CHAR_NOT_NL =>
        rune(ANY_RUNE_NOT_NL, 0)
      case ROP.ANY_CHAR =>
        rune(ANY_RUNE, 0)
      case ROP.BEGIN_LINE =>
        empty(Utils.EMPTY_BEGIN_LINE)
      case ROP.END_LINE =>
        empty(Utils.EMPTY_END_LINE)
      case ROP.BEGIN_TEXT =>
        empty(Utils.EMPTY_BEGIN_TEXT)
      case ROP.END_TEXT =>
        empty(Utils.EMPTY_END_TEXT)
      case ROP.WORD_BOUNDARY =>
        empty(Utils.EMPTY_WORD_BOUNDARY)
      case ROP.NO_WORD_BOUNDARY =>
        empty(Utils.EMPTY_NO_WORD_BOUNDARY)
      case ROP.CAPTURE =>
        val bra: Compiler.Frag = cap(re.cap << 1)
        val sub: Compiler.Frag = compile(re.subs(0))
        val ket: Compiler.Frag = cap(re.cap << 1 | 1)
        cat(cat(bra, sub), ket)
      case ROP.STAR =>
        star(compile(re.subs(0)), (re.flags & RE2.NON_GREEDY) != 0)
      case ROP.PLUS =>
        plus(compile(re.subs(0)), (re.flags & RE2.NON_GREEDY) != 0)
      case ROP.QUEST =>
        quest(compile(re.subs(0)), (re.flags & RE2.NON_GREEDY) != 0)
      case ROP.CONCAT =>
        if (re.subs.length == 0) {
          nop()
        } else {
          val subs: Array[Regexp]    = re.subs
          var f: Frag = null
          var i: Int       = 0
          while (i < subs.length) {
            val sub: Regexp = subs(i)
            val f1: Compiler.Frag  = compile(sub)
            f = if (f == null) f1 else cat(f, f1)
            i += 1
          }
          f
        }
      case ROP.ALTERNATE =>
        if (re.subs.length == 0) {
          nop()
        } else {
          val subs: Array[Regexp]    = re.subs
          var f: Frag = null
          var i: Int       = 0
          while (i < subs.length) {
            val sub: Regexp = subs(i)
            val f1: Compiler.Frag  = compile(sub)
            f = if (f == null) f1 else alt(f, f1)
            i += 1
          }
          f
        }
      case _ =>
        throw new IllegalStateException("regexp: unhandled case in compile")
    }
  }
}

object Compiler {

  /**
   * A fragment of a compiled regular expression program.
   * @see http://swtch.com/~rsc/regexp/regexp1.html
   */
  private class Frag(
      val i: Int, // an instruction address (pc).
      var out: Int // a patch list see explanation in Prog.java
  )

  def compileRegexp(re: Regexp): Prog = {
    val c: Compiler = new Compiler()
    val f: Compiler.Frag = c.compile(re)
    c.prog.patch(f.out, c.newInst(IOP.MATCH).i)
    c.prog.start = f.i
    c.prog
  }

  private val ANY_RUNE_NOT_NL: Array[Int] = Array[Int](
    0,
    '\n' - 1,
    '\n' + 1,
    Unicode.MAX_RUNE
  )

  private val ANY_RUNE: Array[Int] = Array[Int](0, Unicode.MAX_RUNE)
}
