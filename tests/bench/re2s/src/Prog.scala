// Copyright 2010 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Original Go source here:
// http://code.google.com/p/go/source/browse/src/pkg/regexp/syntax/prog.go

package java.util.regex

import java.util.ArrayList
import java.util.regex.Inst.{Op => IOP}

/**
 * A Prog is a compiled regular expression program.
 */
class Prog() {

  private val inst: ArrayList[Inst] = new ArrayList[Inst]()
  var start: Int        = 0 // index of start instruction
  var numCap: Int       = 2 // number of CAPTURE insts in re
  // 2 => implicit ( and ) for whole match $0

  // Returns the instruction at the specified pc.
  // Precondition: pc > 0 && pc < numInst().
  def getInst(pc: Int): Inst = inst.get(pc)

  // Returns the number of instructions in this program.
  def numInst(): Int = inst.size()

  // Adds a new instruction to this program, with operator |op| and |pc| equal
  // to |numInst()|.
  def addInst(op: IOP): Unit = inst.add(new Inst(op))

  // skipNop() follows any no-op or capturing instructions and returns the
  // resulting instruction.
  def skipNop(_pc: Int): Inst = {
    var pc: Int = _pc
    var i: Inst  = inst.get(pc)
    while (i.op == IOP.NOP || i.op == IOP.CAPTURE) {
      i = inst.get(pc)
      pc = i.out
    }
    i
  }

  // prefix() returns a pair of a literal string that all matches for the
  // regexp must start with, and a boolean which is true if the prefix is the
  // entire match.  The string is returned by appending to |prefix|.
  def prefix(prefix: java.lang.StringBuilder): Boolean = {
    var i: Inst = skipNop(start)

    // Avoid allocation of buffer if prefix is empty.
    if (i.runeOp() != IOP.RUNE || i.runes.length != 1) {
      return i.op == IOP.MATCH // (append "" to prefix)
    }

    // Have prefix gather characters.
    while (i.runeOp() == IOP.RUNE &&
           i.runes.length == 1 &&
           (i.arg & RE2.FOLD_CASE) == 0) {
      prefix.appendCodePoint(i.runes(0)) // an int, not a byte.
      i = skipNop(i.out)
    }

    i.op == IOP.MATCH
  }

  // startCond() returns the leading empty-width conditions that must be true
  // in any match.  It returns -1 (all bits set) if no matches are possible.
  def startCond(): Int = {
    var flag: Int  = 0 // bitmask of EMPTY_* flags
    var pc: Int    = start
    var break: Boolean = false
    while (!break) {
      val i: Inst = inst.get(pc)
      i.op match {
        case IOP.EMPTY_WIDTH =>
          flag |= i.arg
        case IOP.FAIL =>
          return -1
        case IOP.CAPTURE | IOP.NOP =>
          () // skip
        case _ =>
          break = true
      }
      if (!break) {
        pc = i.out
      }
    }
    flag
  }

  // --- Patch list ---

  // A patchlist is a list of instruction pointers that need to be filled in
  // (patched).  Because the pointers haven't been filled in yet, we can reuse
  // their storage to hold the list.  It's kind of sleazy, but works well in
  // practice.  See http://swtch.com/~rsc/regexp/regexp1.html for inspiration.

  // These aren't really pointers: they're integers, so we can reinterpret them
  // this way without using package unsafe.  A value l denotes p.inst[l>>1].out
  // (l&1==0) or .arg (l&1==1).  l == 0 denotes the empty list, okay because we
  // start every program with a fail instruction, so we'll never want to point
  // at its output link.

  def next(l: Int): Int = {
    val i: Inst = inst.get(l >> 1)
    if ((l & 1) == 0) {
      i.out
    } else {
      i.arg
    }
  }

  def patch(_l: Int, value: Int): Unit = {
    var l: Int = _l
    while (l != 0) {
      var i: Inst = inst.get(l >> 1)
      if ((l & 1) == 0) {
        l = i.out
        i.out = value
      } else {
        l = i.arg
        i.arg = value
      }
    }
  }

  def append(l1: Int, l2: Int): Int = {
    if (l1 == 0) {
      return l2
    }
    if (l2 == 0) {
      return l1
    }
    var last: Int  = l1
    var break: Boolean = false
    while (!break) {
      val next: Int = this.next(last)
      if (next == 0) {
        break = true
      } else {
        last = next
      }
    }
    val i: Inst = inst.get(last >> 1)
    if ((last & 1) == 0) {
      i.out = l2
    } else {
      i.arg = l2
    }
    l1
  }

  // ---

  override def toString(): String = {
    val out: java.lang.StringBuilder = new java.lang.StringBuilder()
    var pc: Int  = 0
    while (pc < inst.size()) {
      val len: Int = out.length()
      out.append(pc)
      if (pc == start) {
        out.append('*')
      }
      out
        .append("        ".substring(out.length() - len))
        .append(inst.get(pc))
        .append('\n')
      pc += 1
    }
    out.toString()
  }
}
