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

package dotty.tools.dotc.quoted

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.util.NoSource

import scala.quoted.Expr

/* Compilation unit containing the contents of a quoted expression */
class ExprCompilationUnit(val expr: Expr[_]) extends CompilationUnit(NoSource) {
  override def toString: String = s"Expr($expr)"
}
