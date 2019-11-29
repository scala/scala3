
// Show that the static analysis behind flow typing is conservative.

class Test {

  val x: String|Null = ???
  
  // Why is the then branch ok, but the else problematic?
  // The problem is that we're computing a "must not be null analysis".
  // So we know that
  // 1) if the condition x == null && x != null, then both sides of the
  // and must be true. Then it must be the case that x != null, so we 
  // know that x cannot be null and x.length is allowed.
  // Of course, the then branch will never execute, but the analysis doesn't
  // know (so it's ok to say that x won't be null).
  // 2) if the condition is false, then we only know that _one_ or more
  // of the operands failed, but we don't know _which_.
  // This means that we can only pick the flow facts that hold for _both_
  // operands. In particular, we look at x == null, and see that if the condition
  // is false, then x must _not_ be null. But then we look at what happens if
  // x != null is false, and we can't conclude that any variables must be non-null.
  // When we intersect the two sets {x} and \empty, we get the empty set, which
  // correctly approximates reality, which is that we can get to the else branch
  // regardless of whether x is null.

  if (x == null && x != null) {
    val y = x.length // ok
  } else {
    val y = x.length // error
  }

  // Next we show how strengthening the condition can backfire in an
  // unintuitive way.
  if (x != null && 1 == 1) {
    val y = x.length // ok
  }

  if (x == null) {
  } else {
    val y = x.length // ok
  }

  // But
  if (x == null && 1 == 1) { // logically equivalent to `x == null`, but the 
                             // analysis doesn't known
  } else {
    val y = x.length // error
  }

  // The problem here is the same. If the condition is false
  // then we know the l.h.s implies that x must not be null.
  // But the r.h.s doesn't tell us anything about x, so we can't
  // assume that x is non-null. Then the fact that x is non-null can't
  // be propagated to the else branch.
}
