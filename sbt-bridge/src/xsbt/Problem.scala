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

package xsbt

import java.util.Optional
import xsbti.{Position, Severity}

final case class Problem(override val position: Position,
                         override val message: String,
                         override val severity: Severity,
                         rendered0: String) extends xsbti.Problem {
  override val category = ""
  override val rendered = Optional.of(rendered0)
}
