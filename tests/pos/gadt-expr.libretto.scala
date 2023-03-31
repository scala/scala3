// A minimisation of a libretto compilation failure
// that came while implementing GadtExpr
// which induced a type parameter reference pickling leak
// due to how, while constraining the pattern types,
// to assign any type variable constraint or GADT constraints
// TypeComparer unwraps (shallowly) a TypeVar to the origin type parameter ref
// which are the parameter and variable for a pattern bind symbol.
// When the type variable is then instantiated, that doesn't assign in the GADT bound
// so it leaks.
// This was fixed by guarding the TypeVar unwrapping
// to allow the GADT constraints to record
// from 3rd/4th try to 1st/2nd try.
class Rec[F[_]]

sealed trait Foo[A]
case class Bar[F[_]]() extends Foo[F[Rec[F]]]

class Test:
  def meth[X](foo: Foo[X]) = foo match
    case Bar() =>
