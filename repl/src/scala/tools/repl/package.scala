package scala.tools
package repl

import dotty.tools.dotc.reporting.{HideNonSensicalMessages, StoreReporter, UniqueMessagePositions}

/** Create empty outer store reporter */
private[repl] def newStoreReporter: StoreReporter =
  new StoreReporter(null)
  with UniqueMessagePositions with HideNonSensicalMessages
