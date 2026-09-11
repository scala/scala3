package dotty.tools
package repl

import scala.language.unsafeNulls

import org.junit.Test

/** Tab completion with `-Yexplicit-nulls` enabled. */
class TabcompleteExplicitNullsTests
extends ReplTest(ReplTest.defaultOptions ++ Array("-Yexplicit-nulls")) {

  // Completing on a type whose type argument is a type lambda used to abort
  // completion with `Should not flexify HKTypeLambda(...)` from `FlexibleType.apply`.
  @Test def i9334 = initially {
    assert(tabComplete("class Foo[T]; classOf[Foo].").contains("getName"))
  }
}
