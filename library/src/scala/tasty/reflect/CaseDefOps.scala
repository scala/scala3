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
    def unapply(x: CaseDef): Option[(Pattern, Option[Term], Term)]
  }


  trait TypeCaseDefAPI {
    def pattern(implicit ctx: Context): TypeTree
    def rhs(implicit ctx: Context): TypeTree
  }
  implicit def TypeCaseDefDeco(caseDef: TypeCaseDef): TypeCaseDefAPI

  val TypeCaseDef: TypeCaseDefModule
  abstract class TypeCaseDefModule {
    def unapply(x: TypeCaseDef): Option[(TypeTree, TypeTree)]
  }
}
