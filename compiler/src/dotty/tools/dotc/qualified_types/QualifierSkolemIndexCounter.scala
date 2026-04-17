package dotty.tools.dotc.qualified_types

/** Per-run counter for allocating unique negative `ENodeParamRef` indices
 *  used to represent argument references inside qualifiers.
 *
 *  Indices are allocated starting from `startIndex` (default -100) and
 *  decrementing, to avoid collisions with the negative indices used by
 *  [[QualifierSolver.impliesCommonParams]] for lambda opening (which start
 *  at -1 and go down for the number of lambda params, typically just -1).
 */
final class QualifierSkolemIndexCounter(startIndex: Int = -100):
  private var next: Int = startIndex
  def fresh(): Int =
    val i = next
    next -= 1
    i
