package tests

import language.experimental.captureChecking

// The package object desugars into a nested package clause, which must not lose
// the capture checking enabled by the import above.
package object ccRendering {
  def packageObjectByName(body: -> Unit): Int
    = ???
  def packageObjectFunction(f: Int -> Int): Int
    = ???
}
