package scala.quoted

object Reporting {

  /** Throwable used to stop the expansion of a macro after an error was reported */
  class StopQuotedContext extends Throwable

}
