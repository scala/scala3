// A minimisation of a community build failure in PR 21390
// To see why changing the instantiation direction in interpolateTypeVars
// using the same logic as IsFullyDefined.
class Has[A]
object Has:
  class Union[B, C]
  object Union:
    given HasHasUnion[B0 <: Has[?], C0 <: Has[?]]: Union[B0, C0] = ???

class Lay[+D]:
  def and1[B1 >: D, C1](that: Lay[C1])(using Has.Union[B1, C1]): Lay[B1 & C1] = ???
  def and2[B2 >: D, C2](that: Lay[C2])(using Has.Union[B2, C2]): Lay[B2 & C2] = ???

class J; type X = Has[J]
class K; type Y = Has[K]
class L; type Z = Has[L]

def t1(x: Lay[X], y: Lay[Y], z: Lay[Z]): Lay[X & Y & Z] = x.and1(y).and2(z)

/*

Here's what goes wrong in the tvar instantiation, in method t1:

1)  <== constrainResult(method and1, (using x$2: Union[B1, C1]): Lay[B1 & C1], ?{ and2: ? }) = true
2)  ==> Has.Union[B0, C0] <: Has.Union[B1, C1 := Y]?
3)  <== Has.Union[B0, C0] <: Has.Union[B1, C1 := Y] = OK

1)  B1 >: X                   B2 >: B1 & C1
2)  B1 >: X           C1 := Y B2 >: B1 & Y  B0 <: Has[?] C0 <: Has[?]
3)  B1 >: X <: Has[?] C1 := Y B2 >: B1 & Y  B0 := B1     C0 := Y

1) Check that the result of and1 fits the expected .and2 call, inferring any necessary constraints
2) Initiate the check that calling HasHasUnion matches the needed Has.Union[B1, C1] parameter
3) In inferring that the need B0 := B1 and C0 := Y, we end up inferring B0's `<: Has[?]` on B1.

4a) <== B1.instantiate(fromBelow = true ) = X
4b) <== B1.instantiate(fromBelow = false) = Has[?]
5a) <== B2.instantiate(fromBelow = true)  = X & Y
5b) <== B2.instantiate(fromBelow = true)  = Y
6)  <== constrainResult(method and2, (using x$2: Has.Union[B2, C2]): Lay[B2 & C2], Lay[X & Y & Z]) = true

4a) B2 >: X & Y
4b) B2 >: Y & Has[?]
5a) B2 := X & Y
5b) B2 := Y
6a) B2 >: X & Y  C2 <: Z
6b) B2 >: Y      C2 <: X & Z

4) With the extra upper bound constraint, we end up maximising to Has[?] (4b) instead of minimising to X (4a)
5) Which leads to instantiating B2 to just Y (5b) instead of X & Y (5a)
6) Which leads the constraints from the result of and2 to infer X & Z (6b) instead of just Z (6a)

-- [E007] Type Mismatch Error: tests/pos/i21390.zio.scala:14:73 ------------------------------------
14 |def t1(x: Lay[X], y: Lay[Y], z: Lay[Z]): Lay[X & Y & Z] = x.and1(y).and2(z)
   |                                                                         ^
   |                                                                         Found:    (z : Lay[Z])
   |                                                                         Required: Lay[X & Z]

*/
