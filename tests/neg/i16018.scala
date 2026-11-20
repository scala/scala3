// Fixes #16018: shapes that must stay rejected. The fix only widens
// covariant + upper-bounded and contravariant + lower-bounded wildcards;
// every other combination is unsound.
object Test:

  class Co[+T]
  class Contra[-T]
  class Inv[T]
  trait Holder

  // invariant never widens
  def inv_upper[M](xs: Inv[? <: M]): Inv[M] = xs   // error
  def inv_lower[M](xs: Inv[? >: M]): Inv[M] = xs   // error

  // wrong-direction wildcards do not widen
  def co_lower[M](xs: Co[? >: M]): Co[M] = xs             // error
  def contra_upper[M](xs: Contra[? <: M]): Contra[M] = xs // error

  // wildcard hi is wider than the target
  def co_too_wide(xs: Co[? <: Any]): Co[Holder] = xs     // error

  // boundary: the effective bound admits Holder, not a strict subtype
  class Sub extends Holder
  class CoBounded[+T <: Holder]
  def co_boundary(xs: CoBounded[? <: Any]): CoBounded[Sub] = xs            // error
  class Super
  class HolderS extends Super
  class ContraBounded[-T >: HolderS]
  def contra_boundary(xs: ContraBounded[? >: Nothing]): ContraBounded[Super] = xs // error

  // F-bounded (recursive) parameter stays on the conservative path
  class FBounded[+T <: FBounded[T]]
  def f_bounded[M <: FBounded[M]](xs: Co[FBounded[? <: M]]): Co[FBounded[M]] = xs // error

  // per-position variance is still checked through HKT nesting
  def fn_contra_upper[M, R](xs: Co[Function1[? <: M, R]]): Co[Function1[M, R]] = xs // error
  def fn_co_lower[M, T1](xs: Co[Function1[T1, ? >: M]]): Co[Function1[T1, M]] = xs  // error
  def inv_in_co[M](xs: Co[Inv[? <: M]]): Co[Inv[M]] = xs // error
