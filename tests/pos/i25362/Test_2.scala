package example

opaque type BigTuple <: Repr = Repr
final case class Repr(
  f1: Int, f2: Int, f3: Int, f4: Int, f5: Int,
  f6: Int, f7: Int, f8: Int, f9: Int, f10: Int,
  f11: Int, f12: Int, f13: Int, f14: Int, f15: Int,
  f16: Int, f17: Int, f18: Int, f19: Int, f20: Int,
  f21: Int, f22: Int, f23: Int, f24: Int, f25: Int,
)

class test {
  val merger = TC.derived[BigTuple]
}
