package dotty.tools.dotc
package ast

import util.Positions._
import util.DotClass

/** A base class for things that have positions (currently: modifiers and trees)
 */
abstract class Positioned extends DotClass with Product {

  private[this] var curPos: Position = _

  setPos(initialPos)

  /** The item's position.
   */
  def pos: Position = curPos

  /** Destructively update `curPos` to given position. Also, set any missing
   *  positions in children.
   */
  protected def setPos(pos: Position): Unit = {
    curPos = pos
    if (pos.exists) setChildPositions(pos.toSynthetic)
  }

  /** The envelope containing the item in its entirety. Envelope is different from
   *  `pos` for definitions (instances of MemberDef).
   */
  def envelope: Position = pos.toSynthetic

  /** A positioned item like this one with the position set to `pos`.
   *  if the positioned item is source-derived, a clone is returned.
   *  If the positioned item is synthetic, the position is updated
   *  destructively and the item itself is returned.
   */
  def withPos(pos: Position): this.type = {
    val newpd = (if (pos == curPos || curPos.isSynthetic) this else clone).asInstanceOf[Positioned]
    newpd.setPos(pos)
    newpd.asInstanceOf[this.type]
  }

  def withPos(posd: Positioned): this.type =
    if (posd == null) this else withPos(posd.pos)

  /** This item with a position that's the union of the given `pos` and the
   *  current position.
   */
  def addPos(pos: Position): this.type = withPos(pos union this.pos)

  /** Set position of this tree only, without performing
   *  any checks of consistency with - or updates of - other positions.
   *  Called from Unpickler when entering positions.
   */
  private[dotc] def setPosUnchecked(pos: Position) = curPos = pos

  /** If any children of this node do not have positions, set them to the given position,
   *  and transitively visit their children.
   */
  private def setChildPositions(pos: Position): Unit = {
    def deepSetPos(x: Any): Unit = x match {
      case p: Positioned =>
        if (!p.pos.exists) p.setPos(pos)
      case xs: List[_] =>
        xs foreach deepSetPos
      case _ =>
    }
    var n = productArity
    while (n > 0) {
      n -= 1
      deepSetPos(productElement(n))
    }
  }

  /** The initial, synthetic position. This is usually the union of all positioned children's
   *  envelopes.
   */
  protected def initialPos: Position = {
    var n = productArity
    var pos = NoPosition
    while (n > 0) {
      n -= 1
      productElement(n) match {
        case p: Positioned => pos = pos union p.envelope
        case xs: List[_] => pos = unionPos(pos, xs)
        case _ =>
      }
    }
    pos.toSynthetic
  }

  private def unionPos(pos: Position, xs: List[_]): Position = xs match {
    case (p: Positioned) :: xs1 => unionPos(pos union p.envelope, xs1)
    case _ => pos
  }

  def contains(that: Positioned): Boolean = {
    def isParent(x: Any): Boolean = x match {
      case x: Positioned =>
        x contains that
      case xs: List[_] =>
        xs exists isParent
      case _ =>
        false
    }
    (this eq that) ||
      (this.envelope contains that.pos) && {
        var n = productArity
        var found = false
        while (n > 0 && !found) {
          n -= 1
          found = isParent(productElement(n))
        }
        found
      }
  }

  /** The path from this node to `that` node, represented
   *  as a list starting with `this`, ending with`that` where
   *  every node is a child of its predecessor.
   *  Nil if no such path exists.
   */
  def pathTo(that: Positioned): List[Positioned] = {
    def childPath(it: Iterator[Any]): List[Positioned] =
      if (it.hasNext) {
        val cpath = it.next match {
          case x: Positioned => x.pathTo(that)
          case xs: List[_] => childPath(xs.iterator)
          case _ => Nil
        }
        if (cpath.nonEmpty) cpath else childPath(it)
      } else Nil
    if (this eq that) this :: Nil
    else if (this.envelope contains that.pos) {
      val cpath = childPath(productIterator)
      if (cpath.nonEmpty) this :: cpath else Nil
    } else Nil
  }
}
