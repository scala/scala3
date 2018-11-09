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

  def TypeCaseDefDeco(caseDef: TypeCaseDef): TypeCaseDefAPI = new TypeCaseDefAPI {
    def pattern(implicit ctx: Context): Pattern = caseDef.pat
    def rhs(implicit ctx: Context): Term = caseDef.body
  }

  object TypeCaseDef extends TypeCaseDefExtractor {
    def unapply(x: TypeCaseDef): Some[(TypeTree, TypeTree)] = Some((x.pat, x.body))
  }
}
