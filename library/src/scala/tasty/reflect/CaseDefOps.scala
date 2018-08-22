package scala.tasty
package reflect

/** Tasty reflect case definition */
trait CaseDefOps extends TastyCore {

  trait CaseDefAPI {
    def pattern(implicit ctx: Context): Pattern
    def guard(implicit ctx: Context): Option[Term]
    def rhs(implicit ctx: Context): Term
  }
  implicit def CaseDefDeco(caseDef: CaseDef): CaseDefAPI

  val CaseDef: CaseDefExtractor
  abstract class CaseDefExtractor {
    def unapply(x: CaseDef): Option[(Pattern, Option[Term], Term)]
  }

}
