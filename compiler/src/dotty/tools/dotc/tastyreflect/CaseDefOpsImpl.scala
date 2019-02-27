package dotty.tools.dotc.tastyreflect

import dotty.tools.dotc.ast.tpd

trait CaseDefOpsImpl extends scala.tasty.reflect.CaseDefOps with CoreImpl with Helpers {

  object CaseDef extends CaseDefModule {
    def apply(pattern: Pattern, guard: Option[Term], body: Term)(implicit ctx: Context): CaseDef =
      tpd.CaseDef(pattern, guard.getOrElse(tpd.EmptyTree), body)

    def copy(original: CaseDef)(pattern: Pattern, guard: Option[Term], body: Term)(implicit ctx: Context): CaseDef =
      tpd.cpy.CaseDef(original)(pattern, guard.getOrElse(tpd.EmptyTree), body)

    def unapply(x: CaseDef): Some[(Pattern, Option[Term], Term)] = Some(x.pat, optional(x.guard), x.body)
  }

  object TypeCaseDef extends TypeCaseDefModule {
    def apply(pattern: TypeTree, body: TypeTree)(implicit ctx: Context): TypeCaseDef =
      tpd.CaseDef(pattern, tpd.EmptyTree, body)

    def copy(original: TypeCaseDef)(pattern: TypeTree, body: TypeTree)(implicit ctx: Context): TypeCaseDef =
      tpd.cpy.CaseDef(original)(pattern, tpd.EmptyTree, body)

    def unapply(x: TypeCaseDef): Some[(TypeTree, TypeTree)] = Some((x.pat, x.body))
  }
}
