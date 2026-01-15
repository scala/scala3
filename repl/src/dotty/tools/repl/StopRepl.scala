package dotty.tools
package repl

import scala.annotation.static

class StopRepl

object StopRepl {
  // Needs to be volatile, otherwise changes to this may not get seen by other threads
  // for arbitrarily long periods of time (minutes!)
  @static @volatile private var stop: Boolean = false

  @static def setStop(n: Boolean): Unit = { stop = n }

  /** Check if execution should stop, and throw ThreadDeath if so */
  @static def throwIfReplStopped(): Unit = {
    if (stop) throw new ThreadDeath()
  }
}
