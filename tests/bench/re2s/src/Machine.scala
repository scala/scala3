// Copyright 2010 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Original Go source here:
// http://code.google.com/p/go/source/browse/src/pkg/regexp/exec.go

package java.util.regex

import java.util.ArrayList
import java.util.Arrays
import java.util.List
import java.util.regex.Inst.{Op => IOP}
import java.util.regex.Machine._

// A Machine matches an input string of Unicode characters against an
// RE2 instance using a simple NFA.
//
// Called by RE2.doExecute.
class Machine(re2: RE2) {
  // Compiled program.
  private val prog: Prog = re2.prog

  // Two queues for runq, nextq.
  private val q0: Machine.Queue = new Queue(prog.numInst())
  private val q1: Machine.Queue = new Queue(prog.numInst())

  // pool of available threads
  // Really a stack:
  private val pool: ArrayList[Machine.Thread] = new ArrayList[Thread]()

  // Whether a match was found.
  private var matched: Boolean = false

  // Capture information for the match.
  private var matchcap: Array[Int] =
    new Array[Int](if (prog.numCap < 2) 2 else prog.numCap)

  // init() reinitializes an existing Machine for re-use on a new input.
  def init(ncap: Int): Unit = {
    val iter: java.util.Iterator[Machine.Thread] = pool.iterator()
    while (iter.hasNext()) {
      val t: Machine.Thread = iter.next()
      t.cap = new Array[Int](ncap)
    }
    this.matchcap = new Array[Int](ncap)
  }

  def submatches(): Array[Int] = {
    if (matchcap.length == 0) {
      return Utils.EMPTY_INTS
    }
    val cap: Array[Int] = new Array[Int](matchcap.length)
    System.arraycopy(matchcap, 0, cap, 0, matchcap.length)
    cap
  }

  // alloc() allocates a new thread with the given instruction.
  // It uses the free pool if possible.
  private def alloc(inst: Inst): Thread = {
    val n: Int = pool.size()
    val t: Machine.Thread = if (n > 0) pool.remove(n - 1) else new Thread(matchcap.length)
    t.inst = inst
    t
  }

  // free() returns t to the free pool.
  private def free(t: Thread): Unit =
    pool.add(t)

  // match() runs the machine over the input |in| starting at |pos| with the
  // RE2 Anchor |anchor|.
  // It reports whether a match was found.
  // If so, matchcap holds the submatch information.
  def match_(in: MachineInput, _pos: Int, anchor: Int): Boolean = {
    var pos: Int       = _pos
    val startCond: Int = re2.cond
    if (startCond == Utils.EMPTY_ALL) { // impossible
      return false
    }
    if ((anchor == RE2.ANCHOR_START || anchor == RE2.ANCHOR_BOTH) &&
        pos != 0) {
      return false
    }
    matched = false
    Arrays.fill(matchcap, -1)
    var runq: Machine.Queue   = q0
    var nextq: Machine.Queue  = q1
    var r: Int      = in.step(pos)
    var rune: Int   = r >> 3
    var width: Int  = r & 7
    var rune1: Int  = -1
    var width1: Int = 0
    if (r != MachineInput.EOF) {
      r = in.step(pos + width)
      rune1 = r >> 3
      width1 = r & 7
    }
    var flag: Int = 0 // bitmask of EMPTY_* flags
    if (pos == 0) {
      flag = Utils.emptyOpContext(-1, rune)
    } else {
      flag = in.context(pos)
    }
    var stop: Boolean = false
    while (!stop) {
      if (runq.isEmpty()) {
        if ((startCond & Utils.EMPTY_BEGIN_TEXT) != 0 && pos != 0) {
          // Anchored match, past beginning of text.
          stop = true
        }
        if (matched) {
          // Have match finished exploring alternatives.
          stop = true
        }
        if (!re2.prefix.isEmpty() &&
            rune1 != re2.prefixRune &&
            in.canCheckPrefix()) {
          // Match requires literal prefix fast search for it.
          val advance: Int = in.index(re2, pos)
          if (advance < 0) {
            stop = true
          }
          pos += advance
          r = in.step(pos)
          rune = r >> 3
          width = r & 7
          r = in.step(pos + width)
          rune1 = r >> 3
          width1 = r & 7
        }
      }
      if (!matched && (pos == 0 || anchor == RE2.UNANCHORED)) {
        // If we are anchoring at begin then only add threads that begin
        // at |pos| = 0.
        if (matchcap.length > 0) {
          matchcap(0) = pos
        }
        this.add(runq, prog.start, pos, matchcap, flag, null)
      }
      flag = Utils.emptyOpContext(rune, rune1)
      step(runq,
           nextq,
           pos,
           pos + width,
           rune,
           flag,
           anchor,
           pos == in.endPos())
      if (width == 0) { // EOF
        stop = true
      }
      if (matchcap.length == 0 && matched) {
        // Found a match and not paying attention
        // to where it is, so any match will do.
        stop = true
      }
      pos += width
      rune = rune1
      width = width1
      if (rune != -1) {
        r = in.step(pos + width)
        rune1 = r >> 3
        width1 = r & 7
      }
      val tmpq: Machine.Queue = runq
      runq = nextq
      nextq = tmpq
    }
    nextq.clear(pool)
    matched
  }

  // step() executes one step of the machine, running each of the threads
  // on |runq| and appending new threads to |nextq|.
  // The step processes the rune |c| (which may be -1 for EOF),
  // which starts at position |pos| and ends at |nextPos|.
  // |nextCond| gives the setting for the EMPTY_* flags after |c|.
  // |anchor| is the anchoring flag and |atEnd| signals if we are at the end of
  // the input string.
  private def step(runq: Queue,
                   nextq: Queue,
                   pos: Int,
                   nextPos: Int,
                   c: Int,
                   nextCond: Int,
                   anchor: Int,
                   atEnd: Boolean): Unit = {
    val longest: Boolean = re2.longest
    var j: Int       = 0
    while (j < runq.size) {
      val entry: Machine.Entry = runq.dense(j)
      if (entry == null) {
        () // continue
      } else {
        var t: Machine.Thread = entry.thread
        if (t == null) {
          () //continue
        } else {
          if (longest && matched && t.cap.length > 0 && matchcap(0) < t.cap(0)) {
            // free(t)
            pool.add(t)
            () // continue
          } else {
            val i: Inst   = t.inst
            var add: Boolean = false
            i.op match {
              case IOP.MATCH =>
                if (anchor == RE2.ANCHOR_BOTH && !atEnd) {
                  // Don't match if we anchor at both start and end and those
                  // expectations aren't met.
                  () // break switch
                } else {
                  if (t.cap.length > 0 && (!longest || !matched || matchcap(1) < pos)) {
                    t.cap(1) = pos
                    System.arraycopy(t.cap, 0, matchcap, 0, t.cap.length)
                  }
                  if (!longest) {
                    // First-match mode: cut off all lower-priority threads.
                    var k: Int = j + 1
                    while (k < runq.size) {
                      val d: Machine.Entry = runq.dense(k)
                      if (d.thread != null) {
                        // free(d.thread)
                        pool.add(d.thread)
                      }
                      k += 1
                    }
                    runq.size = 0
                  }
                  matched = true
                }
              case IOP.RUNE =>
                add = i.matchRune(c)
              case IOP.RUNE1 =>
                add = c == i.runes(0)
              case IOP.RUNE_ANY =>
                add = true
              case IOP.RUNE_ANY_NOT_NL =>
                add = c != '\n'
              case _ =>
                throw new IllegalStateException("bad inst")
            }
            if (add) {
              t = this.add(nextq, i.out, nextPos, t.cap, nextCond, t)
            }
            if (t != null) {
              // free(t)
              pool.add(t)
            }
          }
        }
      }
      j += 1
    }
    runq.size = 0
  }

  // add() adds an entry to |q| for |pc|, unless the |q| already has such an
  // entry.  It also recursively adds an entry for all instructions reachable
  // from |pc| by following empty-width conditions satisfied by |cond|.  |pos|
  // gives the current position in the input.  |cond| is a bitmask of EMPTY_*
  // flags.
  private def add(q: Queue,
                  pc: Int,
                  pos: Int,
                  cap: Array[Int],
                  cond: Int,
                  _t: Thread): Thread = {
    var t: Machine.Thread = _t
    if (pc == 0) {
      return t
    }
    if (q.contains(pc)) {
      return t
    }
    val d: Machine.Entry    = q.add(pc)
    val inst: Inst = prog.getInst(pc)
    inst.runeOp() match {
      case IOP.FAIL =>
        () // nothing
      case IOP.ALT | IOP.ALT_MATCH =>
        t = this.add(q, inst.out, pos, cap, cond, t)
        t = this.add(q, inst.arg, pos, cap, cond, t)
      case IOP.EMPTY_WIDTH =>
        if ((inst.arg & ~cond) == 0) {
          t = this.add(q, inst.out, pos, cap, cond, t)
        }
      case IOP.NOP =>
        t = this.add(q, inst.out, pos, cap, cond, t)
      case IOP.CAPTURE =>
        if (inst.arg < cap.length) {
          val opos: Int = cap(inst.arg)
          cap(inst.arg) = pos
          this.add(q, inst.out, pos, cap, cond, null)
          cap(inst.arg) = opos
        } else {
          t = this.add(q, inst.out, pos, cap, cond, t)
        }
      case IOP.MATCH | IOP.RUNE | IOP.RUNE1 | IOP.RUNE_ANY |
          IOP.RUNE_ANY_NOT_NL =>
        if (t == null) {
          t = alloc(inst)
        } else {
          t.inst = inst
        }
        if (cap.length > 0 && t.cap != cap) {
          System.arraycopy(cap, 0, t.cap, 0, cap.length)
        }
        d.thread = t
        t = null
      case _ =>
        throw new IllegalStateException("unhandled")
    }
    t
  }
}

object Machine {

  // A logical thread in the NFA.
  private class Thread(n: Int) {
    var cap: Array[Int] = new Array[Int](n)
    var inst: Inst      = null
  }

  // A queue is a 'sparse array' holding pending threads of execution.  See:
  // research.swtch.com/2008/03/using-uninitialized-memory-for-fun-and.html
  private class Queue(n: Int) {
    val dense: Array[Entry] = new Array[Entry](n) // may contain stale Entries in slots >= size
    val sparse: Array[Int]  = new Array[Int](n) // may contain stale but in-bounds values.
    var size: Int           = 0 // of prefix of |dense| that is logically populated

    def contains(pc: Int): Boolean = {
      val j: Int = sparse(pc) // (non-negative)
      if (j >= size) {
        return false
      }
      val d: Machine.Entry = dense(j)
      d != null && d.pc == pc
    }

    def isEmpty(): Boolean = size == 0

    def add(pc: Int): Entry = {
      val j: Int = size
      size += 1
      sparse(pc) = j
      var e: Machine.Entry = dense(j)
      if (e == null) { // recycle previous Entry if any
        val entry: Machine.Entry = new Entry()
        dense(j) = entry
        e = entry
      }
      e.thread = null
      e.pc = pc
      e
    }

    // Frees all threads on the thread queue, returning them to the free pool.
    def clear(freePool: List[Thread]): Unit = {
      var i: Int = 0
      while (i < size) {
        val entry: Machine.Entry = dense(i)
        if (entry != null && entry.thread != null) {
          // free(entry.thread)
          freePool.add(entry.thread)
        }
        // (don't release dense[i] to GC recycle it.)
        i += 1
      }
      size = 0
    }

    override def toString(): String = {
      val out: java.lang.StringBuilder = new java.lang.StringBuilder()
      out.append('{')
      var i: Int = 0
      while (i < size) {
        if (i != 0) {
          out.append(", ")
        }
        out.append(dense(i).pc)
        i += 1
      }
      out.append('}')
      out.toString()
    }
  }

  private class Entry() {
    var pc: Int        = 0
    var thread: Thread = null
  }
}
