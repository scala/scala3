import language.experimental.captureChecking
import caps.*

class File extends SharedCapability

// Bounded capture polymorphism: retains inside type-parameter bounds must
// survive PostTyper's CleanupRetains. The `inBound` flag in CleanupRetains
// preserves any CapSet-derived ref found inside a TypeBounds, regardless
// of whether its binder/owner is a named method. See i25830.

// (A) Flat bounded: `[C^, D^ <: {C}]` in a single poly lambda literal.
//     `{C}` in the bound of `D` is a TypeParamRef to the lambda's own
//     PolyType — the binders stack already handles it.
def testFlat() =
  val f = { [C^, D^ <: {C}] => (xs: List[File^{D}]) => xs }
  val a = File()
  val _ : List[File^{a}] = f[{a}, {a}](List[File^{a}](a))

// (B) Outer `def` + inner poly lambda whose bound references the outer
//     def's capset param. `{C}` in the bound is a ref to a *named*
//     method's type param. Without the `inBound` flag, the scope check
//     would erase `{C}` (named methods are normally excluded). With it,
//     bound retains are preserved unconditionally.
def testCurried[C^]: (xs: List[File^{C}]) => List[File^{C}] =
  val f = { [D^ <: {C}] => (xs: List[File^{D}]) => xs }
  f[{C}]

def useCurried() =
  val a = File()
  val _ : List[File^{a}] = testCurried[{a}](List[File^{a}](a))

// (C) Bound references an outer-class capset param.
class Holder[OuterC^]:
  val mk = { [D^ <: {OuterC}] => (xs: List[File^{D}]) => xs }

def useHolder() =
  val a = File()
  val h = new Holder[{a}]
  val _ : List[File^{a}] = h.mk[{a}](List[File^{a}](a))
