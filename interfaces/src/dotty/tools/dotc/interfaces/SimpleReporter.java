/*
 * Dotty (https://dotty.epfl.ch/)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
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
