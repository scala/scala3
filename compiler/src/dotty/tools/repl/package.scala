package dotty.tools

import dotc.reporting.{HideNonSensicalMessages, StoreReporter, UniqueMessagePositions}

package object repl {
  /** Create empty outer store reporter */
  private[repl] def newStoreReporter: StoreReporter =
    new StoreReporter(null)
    with UniqueMessagePositions with HideNonSensicalMessages
}
