package scala.quoted.runtime

import language.experimental.captureChecking

/** Throwable used to abort the expansion of a macro after an error was reported. */
class StopMacroExpansion extends Throwable:

  // Do not fill the stacktrace for performance.
  // We know that the stacktrace will be ignored
  // and only the reported error message will be used.
  override def fillInStackTrace(): Throwable = this
