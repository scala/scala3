package dotty.tools
package repl

import dotc.reporting.{HideNonSensicalMessages, StoreReporter, UniqueMessagePositions}

/** Create empty outer store reporter */
private[repl] def newStoreReporter: StoreReporter =
  new StoreReporter(null)
  with UniqueMessagePositions with HideNonSensicalMessages
