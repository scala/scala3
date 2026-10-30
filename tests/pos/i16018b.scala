// Fixes #16018: the existential-widening directions the fix legitimizes.
// Covariant `F[? <: hi] <: F[hi]` and contravariant `G[? >: lo] <: G[lo]`,
// the declared-bound intersection/union guard, and the same through HKT.
object Test:

  class Co[+T]
  class Contra[-T]
  trait Holder

  def co_upper[M](xs: Co[? <: M]): Co[M] = xs
  def contra_lower[M](xs: Contra[? >: M]): Contra[M] = xs

  // wildcard hi (Any) intersected with the declared bound: Any & Holder = Holder
  class CoBounded[+T <: Holder]
  def co_declared_bound(xs: CoBounded[? <: Any]): CoBounded[Holder] = xs
  // dual: wildcard lo (Nothing) unioned with the declared lower bound
  class ContraBounded[-T >: Holder]
  def contra_declared_bound(xs: ContraBounded[? >: Nothing]): ContraBounded[Holder] = xs

  // mixed variance through HKT nesting
  def fn_in_co[M, R](xs: Co[Function1[? >: M, R]]): Co[Function1[M, R]] = xs

  // intersection and path-dependent upper bounds compose with the fix
  trait A; trait B
  def inter_upper(xs: Co[? <: A & B]): Co[A] = xs
  trait Outer { type T }
  def path_dep(o: Outer)(xs: Co[? <: o.T]): Co[o.T] = xs

  // the recursive-bound guard rejects only self-referential bounds; an
  // ordinary dependent bound still widens
  class DepBound[+T <: U, U]
  def acyclic_dependent[M](xs: DepBound[? <: M, M]): DepBound[M, M] = xs
