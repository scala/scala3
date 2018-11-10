package dotty.tools.dotc.tastyreflect


trait CaseDefOpsImpl extends scala.tasty.reflect.CaseDefOps with CoreImpl with Helpers {

  def CaseDefDeco(caseDef: CaseDef): CaseDefAPI = new CaseDefAPI {
    def pattern(implicit ctx: Context): Pattern = caseDef.pat
    def guard(implicit ctx: Context): Option[Term] = optional(caseDef.guard)
    def rhs(implicit ctx: Context): Term = caseDef.body
  }

  object CaseDef extends CaseDefExtractor {
    def unapply(x: CaseDef): Some[(Pattern, Option[Term], Term)] = Some(x.pat, optional(x.guard), x.body)
  }

}
