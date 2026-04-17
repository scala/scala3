package dotty.tools.dotc.qualified_types

/** Per-run counter for allocating unique indices for `ENodeVar`s of kind
 *  [[ENodeVarKind.Skolem]] — used to represent argument references inside
 *  qualifiers. Each argument tree that gets skolemized gets a fresh index
 *  stored in a sticky attachment (see
 *  [[QualifiedTypes.QualifierSkolemIndex]]).
 */
final class QualifierSkolemIndexCounter:
  private var next: Int = 0
  def fresh(): Int =
    val i = next
    next += 1
    i
