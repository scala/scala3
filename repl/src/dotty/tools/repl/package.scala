package dotty.tools

import dotc.reporting.{
  StoreReporter,
  UniqueMessagePositions,
  HideNonSensicalMessages
}

package object repl {
  /** Create empty outer store reporter */
  private[repl] def storeReporter: StoreReporter =
    new StoreReporter(null)
    with UniqueMessagePositions with HideNonSensicalMessages
}
