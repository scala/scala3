// Binary search, adapted from the Liquid Types paper (PLDI '08).
//
// The mid-point `(lo + hi) / 2` overflows for large arrays (Bloch's classic
// binary search bug), so under the bitvector semantics of Scala's `Int` the
// mid-point can be negative and every obligation on `i` is invalid: the
// solver must reject them all. Solvers reasoning over ideal integers accept
// this program.
//
// The window `[lo, hi]` may be empty (`hi == lo - 1`), which the terminal
// recursive calls rely on. The initial call typechecks: `posMinusOne`
// encapsulates the offset step `length > 0 ==> -1 <= length - 1 < length`
// behind a runtime check, and its result qualifier matches the window bounds
// at `lo == 0` verbatim.

type NonNeg = {v: Int with v >= 0}

abstract class IntArray(val length: NonNeg, init: Int):
  def access(i: {v: Int with 0 <= v && v < length}): Int

def posMinusOne(x: {v: Int with v > 0}): {v: Int with -1 <= v && v < x} =
  (x - 1).runtimeChecked

def binarySearch(arr: IntArray, x: Int): Boolean =
  def rec(lo: NonNeg)(hi: {v: Int with lo - 1 <= v && v < arr.length}): Boolean =
    if lo <= hi then
      val i = (lo + hi) / 2
      val y = arr.access(i) // error
      if x == y then true
      else if x < y then rec(lo)(i - 1) // error
      else rec(i + 1)(hi) // error // error
    else false
  if arr.length > 0 then rec(0)(posMinusOne(arr.length))
  else false
