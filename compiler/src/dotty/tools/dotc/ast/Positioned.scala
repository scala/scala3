package dotty.tools
package dotc
package ast

import util.Spans._
import util.{SourceFile, SourcePosition}
import core.Contexts.{Context, SourceInfo}
import core.Decorators._
import core.Flags.{JavaDefined, Extension}
import core.StdNames.nme
import io.AbstractFile
import annotation.transientParam
import annotation.internal.sharable

/** A base class for things that have positions (currently: modifiers and trees)
 */
abstract class Positioned(implicit @transientParam src: SourceInfo) extends Product with Cloneable {

  /** A unique identifier. Among other things, used for determining the source file
   *  component of the position.
   */
  private var myUniqueId: Int = _
  private[this] var curPos: Span = _

  /** The item's position */
  def pos: Span = curPos

  /** item's id */
  def uniqueId: Int = myUniqueId

  protected def srcfile: AbstractFile = TreeIds.fileOfId(uniqueId)
  def source(implicit ctx: Context): SourceFile = ctx.getSource(srcfile)
  def sourcePos(implicit ctx: Context): SourcePosition = source.atPos(pos)

  setId(TreeIds.nextIdFor(initialFile(src)))
  setPos(initialSpan(), srcfile)

  protected def setId(id: Int): Unit = {
    myUniqueId = id
    //assert(id != 2067, getClass)
  }

  /** Destructively update `curPos` to given position. Also, set any missing
   *  positions in children.
   */
  protected def setPos(pos: Span, file: AbstractFile): Unit = {
    setPosUnchecked(pos, file)
    if (pos.exists) setChildPositions(pos.toSynthetic, file)
  }

  /** A positioned item like this one with the position set to `pos`.
   *  if the positioned item is source-derived, a clone is returned.
   *  If the positioned item is synthetic, the position is updated
   *  destructively and the item itself is returned.
   */
  def withSpan(pos: Span): this.type = {
    val ownPos = this.pos
    val newpd: this.type =
      if (pos == ownPos || ownPos.isSynthetic) this else cloneIn(srcfile)
    newpd.setPos(pos, srcfile)
    newpd
  }

  def withPosOf(posd: Positioned): this.type = {
    val ownPos = this.pos
    val newpd: this.type =
      if (posd.srcfile == srcfile && posd.pos == ownPos || ownPos.isSynthetic) this
      else cloneIn(posd.srcfile)
    newpd.setPos(posd.pos, posd.srcfile)
    newpd
  }

  def withSourcePos(sourcePos: SourcePosition): this.type = {
    val ownPos = this.pos
    val newpd: this.type =
      if (sourcePos.source.file == srcfile && sourcePos.pos == ownPos || ownPos.isSynthetic) this
      else cloneIn(sourcePos.source.file)
    newpd.setPos(sourcePos.pos, sourcePos.source.file)
    newpd
  }

  /** Set position of this tree only, without performing
   *  any checks of consistency with - or updates of - other positions.
   *  Called from Unpickler when entering positions.
   */
  private[dotc] def setPosUnchecked(pos: Span, file: AbstractFile = this.srcfile): Unit = {
    if (file != this.srcfile) setId(TreeIds.nextIdFor(file))
    curPos = pos
  }

  /** If any children of this node do not have positions,
   *  fit their positions between the positions of the known subtrees
   *  and transitively visit their children.
   *  The method is likely time-critical because it is invoked on any node
   *  we create, so we want to avoid object allocations in the common case.
   *  The method is naturally expressed as two mutually (tail-)recursive
   *  functions, one which computes the next element to consider or terminates if there
   *  is none and the other which propagates the position information to that element.
   *  But since mutual tail recursion is not supported in Scala, we express it instead
   *  as a while loop with a termination by return in the middle.
   */
  private def setChildPositions(pos: Span, file: AbstractFile): Unit = {
    var n = productArity                    // subnodes are analyzed right to left
    var elems: List[Any] = Nil              // children in lists still to be considered, from right to left
    var end = pos.end                       // the last defined offset, fill in positions up to this offset
    var outstanding: List[Positioned] = Nil // nodes that need their positions filled once a start position
                                            // is known, from left to right.
    def fillIn(ps: List[Positioned], start: Int, end: Int): Unit = ps match {
      case p :: ps1 =>
        // If a tree has no position or a zero-extent position, it should be
        // synthetic. We can preserve this invariant by always setting a
        // zero-extent position for these trees here.
        if (!p.pos.exists || p.pos.isZeroExtent) {
          p.setPos(Span(start, start), file)
          fillIn(ps1, start, end)
        } else {
          p.setPos(Span(start, end), file)
          fillIn(ps1, end, end)
        }
      case nil =>
    }
    while (true) {
      var nextChild: Any = null // the next child to be considered
      if (elems.nonEmpty) {
        nextChild = elems.head
        elems = elems.tail
      }
      else if (n > 0) {
        n = n - 1
        nextChild = productElement(n)
      }
      else {
        fillIn(outstanding, pos.start, end)
        return
      }
      nextChild match {
        case p: Positioned =>
          if (p.pos.exists) {
            fillIn(outstanding, p.pos.end, end)
            outstanding = Nil
            end = p.pos.start
          }
          else outstanding = p :: outstanding
        case m: untpd.Modifiers =>
          elems = elems ::: m.mods.reverse ::: m.annotations.reverse
        case xs: List[_] =>
          elems = elems ::: xs.reverse
        case _ =>
      }
    }
  }

  protected def cloneIn(file: AbstractFile): this.type = {
    val newpd: this.type = clone.asInstanceOf[this.type]
    newpd.setId(TreeIds.nextIdFor(file))
    newpd
  }

  def elemsFile: AbstractFile = {
    def firstFile(x: Any): AbstractFile = x match {
      case x: Positioned if x.pos.exists =>
        x.srcfile
      case x1 :: xs1 =>
        val f = firstFile(x1)
        if (f != null) f else firstFile(xs1)
      case _ =>
        null
    }
    def firstElemFile(n: Int): AbstractFile =
      if (n == productArity) null
      else {
        val f = firstFile(productElement(n))
        if (f != null) f else firstElemFile(n + 1)
      }
    firstElemFile(0)
  }

  private def initialFile(src: SourceInfo): AbstractFile = {
    val f = elemsFile
    if (f != null) f else src.srcfile
  }

  /** The initial, synthetic span. This is usually the union of all positioned children's positions.
   *  @param  ignoreTypeTrees  If true, TypeTree children are not counted for the span.
   *                           This is important for predicting whether a position entry is
   *                           needed for pickling, since TypeTrees are pickled as types, so
   *                           their position is lost.
   */
  def initialSpan(ignoreTypeTrees: Boolean = false): Span = {

    def include(p1: Span, p2: Positioned) = p2 match {
      case _: Trees.TypeTree[_] if ignoreTypeTrees => p1
      case _ => p1.union(p2.pos)
    }

    def includeAll(pos: Span, xs: List[_]): Span = xs match {
      case (p: Positioned) :: xs1 if sameSource(p) => includeAll(include(pos, p), xs1)
      case (xs0: List[_]) :: xs1 => includeAll(includeAll(pos, xs0), xs1)
      case _ :: xs1 => includeAll(pos, xs1)
      case _ => pos
    }

    var n = productArity
    var pos = NoSpan
    while (n > 0) {
      n -= 1
      productElement(n) match {
        case p: Positioned if sameSource(p) => pos = include(pos, p)
        case m: untpd.Modifiers => pos = includeAll(includeAll(pos, m.mods), m.annotations)
        case xs: List[_] => pos = includeAll(pos, xs)
        case _ =>
      }
    }
    pos.toSynthetic
  }

  private def sameSource(that: Positioned) = srcfile == that.srcfile

  def contains(that: Positioned): Boolean = {
    def isParent(x: Any): Boolean = x match {
      case x: Positioned =>
        x.contains(that)
      case m: untpd.Modifiers =>
        m.mods.exists(isParent) || m.annotations.exists(isParent)
      case xs: List[_] =>
        xs.exists(isParent)
      case _ =>
        false
    }
    (this eq that) ||
      (this.pos contains that.pos) && {
        var n = productArity
        var found = false
        while (!found && n > 0) {
          n -= 1
          found = isParent(productElement(n))
        }
        found
      }
  }

  /** Check that all positioned items in this tree satisfy the following conditions:
   *  - Parent positions contain child positions
   *  - If item is a non-empty tree, it has a position
   */
  def checkPos(nonOverlapping: Boolean)(implicit ctx: Context): Unit = try {
    import untpd._
    var lastPositioned: Positioned = null
    var lastPos = NoSpan
    def check(p: Any): Unit = p match {
      case p: Positioned =>
        assert(pos contains p.pos,
          s"""position error, parent position does not contain child position
             |parent          = $this,
             |parent position = $pos,
             |child           = $p,
             |child position  = ${p.pos}""".stripMargin)
        p match {
          case tree: Tree if !tree.isEmpty =>
            assert(tree.pos.exists,
              s"position error: position not set for $tree # ${tree.uniqueId}")
          case _ =>
        }
        if (nonOverlapping) {
          this match {
            case _: XMLBlock =>
              // FIXME: Trees generated by the XML parser do not satisfy `checkPos`
            case _: WildcardFunction
            if lastPositioned.isInstanceOf[ValDef] && !p.isInstanceOf[ValDef] =>
              // ignore transition from last wildcard parameter to body
            case _ =>
              assert(!lastPos.exists || !p.pos.exists || lastPos.end <= p.pos.start,
                s"""position error, child positions overlap or in wrong order
                   |parent             = $this
                   |1st child          = $lastPositioned
                   |1st child position = $lastPos
                   |2nd child          = $p
                   |2nd child position = ${p.pos}""".stripMargin)
          }
          lastPositioned = p
          lastPos = p.pos
        }
        p.checkPos(nonOverlapping)
      case m: untpd.Modifiers =>
        m.annotations.foreach(check)
        m.mods.foreach(check)
      case xs: List[_] =>
        xs.foreach(check)
      case _ =>
    }
    this match {
      case tree: DefDef if tree.name == nme.CONSTRUCTOR && tree.mods.is(JavaDefined) =>
        // Special treatment for constructors coming from Java:
        // Leave out tparams, they are copied with wrong positions from parent class
        check(tree.mods)
        check(tree.vparamss)
      case tree: DefDef if tree.mods.is(Extension) =>
        tree.vparamss match {
          case vparams1 :: vparams2 :: rest if !isLeftAssoc(tree.name) =>
            check(vparams2)
            check(tree.tparams)
            check(vparams1)
            check(rest)
          case vparams1 :: rest =>
            check(vparams1)
            check(tree.tparams)
            check(rest)
          case _ =>
            check(tree.tparams)
        }
        check(tree.tpt)
        check(tree.rhs)
      case _ =>
        val end = productArity
        var n = 0
        while (n < end) {
          check(productElement(n))
          n += 1
        }
    }
  } catch {
    case ex: AssertionError =>
      println(i"error while checking $this")
      throw ex
  }
}
