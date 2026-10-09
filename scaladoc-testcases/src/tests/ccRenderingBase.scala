package tests
package ccRenderingBase

import language.experimental.captureChecking

// Not documented itself: the members below are expected as inherited members of
// `tests.ccRendering.NonCCChild`, which is compiled without capture checking.
trait CCBase: //unexpected
  def pureByName(x: -> Int): Int
  def impureByName(x: => Int): Int
  def pureFunction(f: Int -> Int): Int
  def impureFunction(f: Int => Int): Int
