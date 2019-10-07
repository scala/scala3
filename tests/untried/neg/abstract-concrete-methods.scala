trait Outer[Self <: Outer[Self]] {
  self: Self =>

  trait Inner
  def score(i: Self#Inner): Double
}
class Outer2 extends Outer[Outer2] {
  class Inner extends super.Inner
  def score(i: Outer2#Inner) = 0.0
}
