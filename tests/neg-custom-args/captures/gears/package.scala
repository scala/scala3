package gears

import language.experimental.captureChecking

/** Asynchronous programming support with direct-style Scala.
  * @see
  *   [[gears.async.Async]] for an introduction to the [[Async]] context and how to create them.
  * @see
  *   [[gears.async.Future]] for a simple interface to spawn concurrent computations.
  * @see
  *   [[gears.async.Channel]] for a simple inter-future communication primitive.
  */
package object async:
  type CancellationException = java.util.concurrent.CancellationException
