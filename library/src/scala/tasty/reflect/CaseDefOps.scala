package scala.tasty
package reflect

/** Tasty reflect case definition */
trait CaseDefOps extends Core {

  trait CaseDefAPI {
    def pattern(implicit ctx: Context): Pattern
    def guard(implicit ctx: Context): Option[Term]
    def rhs(implicit ctx: Context): Term
  }
  implicit def CaseDefDeco(caseDef: CaseDef): CaseDefAPI

  val CaseDef: CaseDefModule
  abstract class CaseDefModule {

    def apply(pattern: Pattern, guard: Option[Term], body: Term)(implicit ctx: Context): CaseDef

    def copy(original: CaseDef)(pattern: Pattern, guard: Option[Term], body: Term)(implicit ctx: Context): CaseDef

    def unapply(x: CaseDef): Option[(Pattern, Option[Term], Term)]
  }


  trait TypeCaseDefAPI {
    def pattern(implicit ctx: Context): TypeTree
    def rhs(implicit ctx: Context): TypeTree
  }
  implicit def TypeCaseDefDeco(caseDef: TypeCaseDef): TypeCaseDefAPI

  val TypeCaseDef: TypeCaseDefModule
  abstract class TypeCaseDefModule {
    def apply(pattern: TypeTree, body: TypeTree)(implicit ctx: Context): TypeCaseDef

    def copy(original: TypeCaseDef)(pattern: TypeTree, body: TypeTree)(implicit ctx: Context): TypeCaseDef

    def unapply(x: TypeCaseDef): Option[(TypeTree, TypeTree)]
  }
}
