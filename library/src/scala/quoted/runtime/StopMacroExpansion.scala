package scala.quoted.runtime

import language.experimental.captureChecking

/** Throwable used to abort the expansion of a macro after an error was reported. */
class StopMacroExpansion extends Throwable:

  // Do not fill the stacktrace for performance.
  // We know that the stacktrace will be ignored
  // and only the reported error message will be used.
  /** Returns `this` without filling in the stack trace, since the trace is ignored: the compiler
   *  reports the error that was already emitted, or, if the macro threw this without reporting
   *  one, a diagnostic saying the expansion aborted without an error.
   */
  override def fillInStackTrace(): Throwable = this
