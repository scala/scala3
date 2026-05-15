package dotty.tools.dotc.qualified_types

/** Per-owner counter for allocating unique indices for
 *  [[ENodeVar.Skolem]] — used to represent argument references inside
 *  qualifiers. Each skolemized argument or local val gets a fresh
 *  index stored as a `@scala.annotation.internal.QualifierSkolemIndex(n)`
 *  annotation, either on the argument tree's type (via
 *  [[QualifiedTypes.wrapWithSkolemIndex]]) or on the symbol (for
 *  EtaExpansion-lifted vals).
 */
final class QualifierSkolemIndexCounter:
  private var next: Int = 0
  def fresh(): Int =
    val i = next
    next += 1
    i
