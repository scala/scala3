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

package dotty.tools.dotc
package transform

import core._
import Names._
import dotty.tools.dotc.transform.MegaPhase._
import ast.Trees._
import ast.untpd
import Flags._
import Types._
import Constants.Constant
import Contexts.Context
import Symbols._
import Decorators._
import Annotations._
import Annotations.ConcreteAnnotation
import Denotations.SingleDenotation
import SymDenotations.SymDenotation
import scala.collection.mutable
import DenotTransformers._
import NameOps._
import NameKinds.OuterSelectName
import StdNames._

object ElimOpaque {
  val name: String = "elimOpaque"
}

/** Rewrites opaque type aliases to normal alias types */
class ElimOpaque extends MiniPhase with SymTransformer {

  override def phaseName: String = ElimOpaque.name

  // Override checks need to take place before treating opaque types as aliases
  override def runsAfterGroupsOf: Set[String] = Set(typer.RefChecks.name)

  // base types of opaque aliases change
  override def changesBaseTypes = true

  def transformSym(sym: SymDenotation)(implicit ctx: Context): SymDenotation =
    if (sym.isOpaqueHelper) {
      sym.copySymDenotation(
        info = TypeAlias(sym.opaqueAlias),
        initFlags = sym.flags &~ (Opaque | Deferred))
    }
    else sym
}