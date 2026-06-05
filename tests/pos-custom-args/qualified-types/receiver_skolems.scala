// Receivers of dependent members on unstable prefixes. The receiver skolem index
// is remembered on the receiver tree (`@QualifierSkolemIndex`), so the type is
// stable across re-typing/TASTy, and under `-Yqualified-types-anf` the ANF phase
// lifts the receiver into a `val` like any other dependent argument. This checks
// both that such receivers are handled and that ordinary selections are untouched.

import scala.language.experimental.qualifiedTypes

class Counter(val max: Int):
  // Result qualifier depends on `this` (the receiver).
  def peek: {r: Int with r <= this.max} = 0.runtimeChecked
  // Parameter qualifier depends on `this`.
  def step(n: {r: Int with r <= this.max}): Int = n
  // Field whose type's qualifier depends on `this`.
  val cap: {r: Int with r <= this.max} = 0.runtimeChecked
  // Qualified, but does NOT mention `this`.
  def positive: {r: Int with r > 0} = 1.runtimeChecked
  // No qualifier at all.
  def plain: Int = 1

// Unstable factory: every call is a fresh, non-stable receiver.
def mk(n: Int): {c: Counter with c.max == n} = Counter(n).runtimeChecked

def tests(): Unit =
  // Applied dependent method on an unstable receiver.
  mk(10).step(0.runtimeChecked)

  // Bare, *unapplied* nullary dependent method.
  mk(10).peek

  // Field selection with a `this`-dependent qualifier.
  mk(10).cap

  // A dependent selection used as a dependent argument: the inner result is
  // lifted into a `val`, then passed to the outer call.
  mk(mk(10).positive).plain

  // Selections that do not depend on `this`: receiver untouched.
  mk(10).positive
  mk(10).plain

  // Stable receiver: never skolemized.
  val c = Counter(5)
  c.peek
  c.step(0.runtimeChecked)
