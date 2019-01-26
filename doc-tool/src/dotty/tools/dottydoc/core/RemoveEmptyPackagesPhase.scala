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

package dotty.tools
package dottydoc
package core

import dotc.core.Contexts.Context

import transform.DocMiniPhase
import model._

class RemoveEmptyPackages extends DocMiniPhase {
  override def transformPackage(implicit ctx: Context) = { case p: Package =>
    if (p.members.exists(_.kind != "package")) p :: Nil
    else Nil
  }
}
