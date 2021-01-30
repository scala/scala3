class C(val x: Int) extends TypeHelpers
abstract class TypeHelpers:
  self: C =>
  def f = x
@main def Test =
  C(0).f
