package dotty.tools.dotc.tastyreflect

import dotty.tools.dotc.ast.tpd

trait CaseDefOpsImpl extends scala.tasty.reflect.CaseDefOps with TastyCoreImpl with Helpers {

  def CaseDefDeco(caseDef: CaseDef): CaseDefAPI = new CaseDefAPI {
    def pattern(implicit ctx: Context): Pattern = caseDef.pat
    def guard(implicit ctx: Context): Option[Term] = optional(caseDef.guard)
    def rhs(implicit ctx: Context): Term = caseDef.body
  }

  object CaseDef extends CaseDefExtractor {
    def unapply(x: CaseDef): Option[(Pattern, Option[Term], Term)] = x match {
      case x: tpd.CaseDef =>
        Some(x.pat, optional(x.guard), x.body)
      case _ => None
    }
  }

}
