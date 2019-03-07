package scala.tasty
package reflect

/** Tasty reflect case definition */
trait CaseDefOps extends Core {

  implicit class CaseDefAPI(caseDef: CaseDef) {
    def pattern(implicit ctx: Context): Pattern = kernel.CaseDef_pattern(caseDef)
    def guard(implicit ctx: Context): Option[Term] = kernel.CaseDef_guard(caseDef)
    def rhs(implicit ctx: Context): Term = kernel.CaseDef_rhs(caseDef)
  }

  object CaseDef {
    def apply(pattern: Pattern, guard: Option[Term], rhs: Term)(implicit ctx: Context): CaseDef =
      kernel.CaseDef_module_apply(pattern, guard, rhs)

    def copy(original: CaseDef)(pattern: Pattern, guard: Option[Term], rhs: Term)(implicit ctx: Context): CaseDef =
      kernel.CaseDef_module_copy(original)(pattern, guard, rhs)

    def unapply(x: CaseDef)(implicit ctx: Context): Option[(Pattern, Option[Term], Term)] =
      Some((x.pattern, x.guard, x.rhs))
  }

  implicit class TypeCaseDefAPI(caseDef: TypeCaseDef) {
    def pattern(implicit ctx: Context): TypeTree = kernel.TypeCaseDef_pattern(caseDef)
    def rhs(implicit ctx: Context): TypeTree = kernel.TypeCaseDef_rhs(caseDef)
  }

  object TypeCaseDef {
    def apply(pattern: TypeTree, rhs: TypeTree)(implicit ctx: Context): TypeCaseDef =
      kernel.TypeCaseDef_module_apply(pattern, rhs)

    def copy(original: TypeCaseDef)(pattern: TypeTree, rhs: TypeTree)(implicit ctx: Context): TypeCaseDef =
      kernel.TypeCaseDef_module_copy(original)(pattern, rhs)

    def unapply(x: TypeCaseDef)(implicit ctx: Context): Option[(TypeTree, TypeTree)] =
      Some((x.pattern, x.rhs))
  }

}
