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

package dotty.tools.dotc.tastyreflect

import dotty.tools.dotc.ast.tpd

trait CaseDefOpsImpl extends scala.tasty.reflect.CaseDefOps with CoreImpl with Helpers {

  def CaseDefDeco(caseDef: CaseDef): CaseDefAPI = new CaseDefAPI {
    def pattern(implicit ctx: Context): Pattern = caseDef.pat
    def guard(implicit ctx: Context): Option[Term] = optional(caseDef.guard)
    def rhs(implicit ctx: Context): Term = caseDef.body
  }

  object CaseDef extends CaseDefModule {
    def apply(pattern: Pattern, guard: Option[Term], body: Term)(implicit ctx: Context): CaseDef =
      tpd.CaseDef(pattern, guard.getOrElse(tpd.EmptyTree), body)

    def copy(original: CaseDef)(pattern: Pattern, guard: Option[Term], body: Term)(implicit ctx: Context): CaseDef =
      tpd.cpy.CaseDef(original)(pattern, guard.getOrElse(tpd.EmptyTree), body)

    def unapply(x: CaseDef): Some[(Pattern, Option[Term], Term)] = Some(x.pat, optional(x.guard), x.body)
  }

  def TypeCaseDefDeco(caseDef: TypeCaseDef): TypeCaseDefAPI = new TypeCaseDefAPI {
    def pattern(implicit ctx: Context): Pattern = caseDef.pat
    def rhs(implicit ctx: Context): Term = caseDef.body
  }

  object TypeCaseDef extends TypeCaseDefModule {
    def apply(pattern: TypeTree, body: TypeTree)(implicit ctx: Context): TypeCaseDef =
      tpd.CaseDef(pattern, tpd.EmptyTree, body)

    def copy(original: TypeCaseDef)(pattern: TypeTree, body: TypeTree)(implicit ctx: Context): TypeCaseDef =
      tpd.cpy.CaseDef(original)(pattern, tpd.EmptyTree, body)

    def unapply(x: TypeCaseDef): Some[(TypeTree, TypeTree)] = Some((x.pat, x.body))
  }
}
