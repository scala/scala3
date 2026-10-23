// Selections on non-path prefixes have `TermRef` types whose prefix is not a
// singleton (e.g. `test.L#rev` below). Selfifying such a reference used to
// crash with "cannot establish a reference" (epfl-lara/stainless#1776).
object test:
  case class L():
    def rev: L = this

  class P:
    val c: 42 = 42

  def foo: Unit =
    val xt: L with false = L().rev // error
    val cn: Int with false = P().c // error
    ()
