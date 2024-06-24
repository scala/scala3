package scala.quoted
package runtime.impl

import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.util.Property
import dotty.tools.dotc.util.SourcePosition

/** A scope uniquely identifies the context for evaluating a splice
 *
 *  A nested splice gets a new scope with the enclosing scope as its `outer`.
 *  This also applies for recursive splices.
 */
trait Scope {
  /** Outer scope that was used to create the quote containing this splice.
   *  NoScope otherwise.
   */
  def outer: Scope = NoScope
  /** Is this is a outer scope of the given scope */
  def isOuterScopeOf(scope: Scope): Boolean =
    this.eq(scope) || (scope.ne(NoScope) && isOuterScopeOf(scope.outer))
  /** Scope of the top level splice or staging `run` */
  def root: Scope =
    if outer.eq(NoScope) then this else outer.root
  /** Stack of locations where scopes where evaluated */
  def stack: List[String] =
    this.toString :: (if outer.eq(NoScope) then Nil else outer.stack)
  /** If the two scopes correspond to the same splice in source. */
  def atSameLocation(scope: Scope): Boolean = false
}

/** Only used for outer scope of top level splice and staging `run` */
object NoScope extends Scope:
  override def root: Scope = this
  override def outer: Scope = throw UnsupportedOperationException("NoScope.outer")

class SpliceScope(val pos: SourcePosition, override val outer: Scope) extends Scope:

  override def atSameLocation(scope: Scope): Boolean = scope match
    case scope: SpliceScope => this.pos == scope.pos
    case _ => false

  override def toString =
    if pos.exists then
      s"${pos.source.toString}:${pos.startLine + 1} at column ${pos.startColumn + 1}"
    else
      "Unknown location"

end SpliceScope


object SpliceScope:

  /** A key to be used in a context property that tracks current splices we are evaluating */
  private val ScopeKey = new Property.Key[Scope]

  def setSpliceScope(scope: Scope)(using Context): Context =
    ctx.fresh.setProperty(ScopeKey, scope)

  def contextWithNewSpliceScope(pos: SourcePosition)(using Context): Context =
    ctx.fresh.setProperty(ScopeKey, new SpliceScope(pos, getCurrent))

    /** Context with an incremented quotation level. */
  def getCurrent(using Context): Scope =
      ctx.property(ScopeKey).getOrElse(NoScope)

end SpliceScope
