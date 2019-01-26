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

/** Summary of the diagnostics emitted by a Reporter.
 *
 *  User code should not implement this interface, but it may have to
 *  manipulate objects of this type.
 */
public interface ReporterResult {
  /** @return Have we emitted any error? */
  boolean hasErrors();
  /** @return Number of errors that have been emitted */
  int errorCount();

  /** @return Have we emitted any warning ? */
  boolean hasWarnings();
  /** @return Number of warnings that have been emitted */
  int warningCount();
}
