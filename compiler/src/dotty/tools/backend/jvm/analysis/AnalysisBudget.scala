package dotty.tools
package backend.jvm
package analysis

import scala.util.control.ControlThrowable
import org.objectweb.asm.tree.MethodNode

/**
 * Bounds the work done by producer/consumer analyses of a single method.
 *
 * The cost of a `SourceValue` analysis is dominated by merging producer sets at control-flow joins
 * (including exception edges), and it grows much faster than method size: large methods with many
 * locals, loops or nested try/finally can take hundreds of milliseconds, while static size limits
 * (see [[dotty.tools.backend.jvm.opt.Limits]]) predict this poorly. Charging the merge work as it
 * happens bounds it directly and deterministically, so the generated code does not depend on timing.
 *
 * When the budget is exhausted, [[charge]] throws [[AnalysisBudget.Exhausted]], and [[run]] abandons the
 * optimization. That is only safe if the optimization has not changed the method yet. The analysis runs to
 * completion on its first query, so it suffices that optimizations complete their analyses before
 * rewriting.
 */
final class AnalysisBudget(limit: Long) {
  // Merge work charged so far, summed over every analysis of the method in every round. It keeps counting
  // past `limit`, so once exhausted the budget stays exhausted and later optimizations are skipped without
  // starting an analysis. A budget belongs to one method and is only used from the thread optimizing it.
  private var used = 0L

  /** The merge work charged so far. */
  def spent: Long = used

  def exhausted: Boolean = used > limit

  def charge(work: Int): Unit = {
    used += work
    if (used > limit) throw AnalysisBudget.Exhausted
  }

  override def toString: String =
    s"AnalysisBudget(spent $used of ${if (limit == Long.MaxValue) "unlimited" else limit.toString})"

  /** A producer/consumer analyzer of `method` that charges this budget. */
  def prodConsAnalyzer(method: MethodNode, owner: String): ProdConsAnalyzer = new ProdConsAnalyzer(method, owner, this)

  /** Runs an optimization of `method` that uses budgeted analyses, returning `noChange` if the budget is exhausted. */
  def run[T](method: MethodNode, noChange: T)(optimization: => T): T =
    if (exhausted) noChange
    else {
      try optimization
      catch { case AnalysisBudget.Exhausted => noChange }
    }
}

object AnalysisBudget {
  object Exhausted extends ControlThrowable

  /** A budget per method, from `-Yopt-analysis-budget`. */
  def apply(limit: Int): AnalysisBudget = new AnalysisBudget(limit.toLong)

  def unlimited: AnalysisBudget = new AnalysisBudget(Long.MaxValue)
}
