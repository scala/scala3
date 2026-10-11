package tests
package ccRenderingLegacy

// Not documented itself: the members below are expected as inherited members of
// `tests.ccRendering.CCChild`, which is compiled with capture checking.
trait LegacyBase: //unexpected
  def legacyByName(x: => Int): Int
  def legacyFunction(f: Int => Int): Int
