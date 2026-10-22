import scala.quoted.*

trait Tactic[+T]

object LemmaMacros:
  def use(state: Int, tactical: Tactic[Any]): Int = state + 1

  inline def applyTactics[U](inline tacticsProof: => Tactic[U]): Int =
    ${ helperImpl('tacticsProof) }

  def helperImpl[U: Type](tacticsProof: Expr[Tactic[U]])(using Quotes): Expr[Int] =
    constructProofState('{ 0 }, tacticsProof)

  private def constructProofState[U: Type](proofState: Expr[Int], tacticsProof: Expr[Tactic[U]])(using Quotes): Expr[Int] =
    import quotes.reflect.*
    def unwrapInlined(term: Term): Term = term match
      case Inlined(_, _, t) => unwrapInlined(t)
      case t => t
    val tacticTerms: List[Term] = unwrapInlined(tacticsProof.asTerm) match
      case Block(stmts, term) => stmts.collect { case t: Term => t } :+ term
      case term => List(term)
    tacticTerms.foldLeft(proofState) { (acc, tactic) =>
      '{ LemmaMacros.use($acc, ${ tactic.asExprOf[Tactic[Any]] }) }
    }
