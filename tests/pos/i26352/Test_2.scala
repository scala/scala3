class Sub extends Outer:
  val v = inner.make

object Mk:
  def mkOuter(): Outer = new Outer {}

// impure prefix: an outer path is built through the nested module `inner`, whose
// outer accessor must be reconstructed in this (separate) compilation unit.
val w = Mk.mkOuter().inner.make
