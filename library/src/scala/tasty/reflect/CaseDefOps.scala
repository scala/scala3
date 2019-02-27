package scala.tasty
package reflect

/** Tasty reflect case definition */
trait CaseDefOps extends Core {

  implicit class CaseDefAPI(caseDef: CaseDef) {
    def pattern(implicit ctx: Context): Pattern = kernel.CaseDef_pattern(caseDef)
    def guard(implicit ctx: Context): Option[Term] = kernel.CaseDef_guard(caseDef)
    def rhs(implicit ctx: Context): Term = kernel.CaseDef_rhs(caseDef)
  }

  val CaseDef: CaseDefModule
  abstract class CaseDefModule {

    def apply(pattern: Pattern, guard: Option[Term], body: Term)(implicit ctx: Context): CaseDef

    def copy(original: CaseDef)(pattern: Pattern, guard: Option[Term], body: Term)(implicit ctx: Context): CaseDef

    def unapply(x: CaseDef): Option[(Pattern, Option[Term], Term)]
  }


  implicit class TypeCaseDefAPI(caseDef: TypeCaseDef) {
    def pattern(implicit ctx: Context): TypeTree = kernel.TypeCaseDef_pattern(caseDef)
    def rhs(implicit ctx: Context): TypeTree = kernel.TypeCaseDef_rhs(caseDef)
  }

  val TypeCaseDef: TypeCaseDefModule
  abstract class TypeCaseDefModule {
    def apply(pattern: TypeTree, body: TypeTree)(implicit ctx: Context): TypeCaseDef

    def copy(original: TypeCaseDef)(pattern: TypeTree, body: TypeTree)(implicit ctx: Context): TypeCaseDef

    def unapply(x: TypeCaseDef): Option[(TypeTree, TypeTree)]
  }
}
