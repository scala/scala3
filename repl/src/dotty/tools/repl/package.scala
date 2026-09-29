package dotty.tools
package repl

import dotc.reporting.{HideNonSensicalMessages, StoreReporter, UniqueMessagePositions}

/** Create empty outer store reporter */
private[repl] def newStoreReporter: StoreReporter = ReplReporter()

private[repl] class ReplReporter extends StoreReporter(null), UniqueMessagePositions, HideNonSensicalMessages
