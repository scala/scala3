// A minimisation of a protoquill test failure
// that came while implementing GadtExpr
// which was causing a unresolved symbols pickler error.
// This is caused because during typedUnapply
// pattern bound symbols are added to the GADT constraint
// that aren't actually part of the unapply
// so it was fixed by giving that its own fresh GADT constraint
class Foo[A]
class Bar[B]:
  def unapply(any: Any): true = true

class Test:
  def test(any: Any) = any match
    case Bar[Foo[?]]() => 1
