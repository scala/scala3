/*
 * Dotty (https://dotty.epfl.ch/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 */

package dotty.tools.dotc.interfaces;

/** Report errors, warnings and info messages during the compilation process
 *
 *  You should implement this interface if you want to handle the diagnostics
 *  returned by the compiler yourself.
 *
 *  See the method `process` of `dotty.tools.dotc.Driver` for more information.
 */
public interface SimpleReporter {
  /** Report a diagnostic.
   *  @param diag the diagnostic message to report
   */
  void report(Diagnostic diag);
}
