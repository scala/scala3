package dotty.tools.languageserver.worksheet

import org.eclipse.lsp4j.jsonrpc.CancelChecker

import java.util.concurrent.CancellationException

/**
 * Regularly check whether execution has been cancelled, kill REPL if it is.
 */
private class CancellationThread(@volatile private[this] var cancelChecker: CancelChecker,
                                 evaluator: Evaluator) extends Thread {
  private final val checkCancelledDelayMs = 50

  override def run(): Unit = {
    try {
      while (evaluator.isAlive() && !Thread.interrupted()) {
        cancelChecker.checkCanceled()
        Thread.sleep(checkCancelledDelayMs)
      }
    } catch {
      case _: CancellationException => evaluator.exit()
      case _: InterruptedException => evaluator.exit()
    }
  }

  def setCancelChecker(cancelChecker: CancelChecker): Unit = {
    this.cancelChecker = cancelChecker
  }
}
