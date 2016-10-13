package dotty.tools
package dotc
package core
package tasty

import ast._
import ast.Trees._
import ast.Trees.WithLazyField
import TastyFormat._
import core._
import Contexts._, Symbols._, Types._, Names._, Constants._, Decorators._, Annotations._
import collection.mutable
import TastyBuffer._
import util.Positions._

class PositionPickler(pickler: TastyPickler, addrOfTree: tpd.Tree => Option[Addr]) {
  val buf = new TastyBuffer(5000)
  pickler.newSection("Positions", buf)
  import buf._
  import ast.tpd._

  def header(addrDelta: Int, hasStartDelta: Boolean, hasEndDelta: Boolean) = {
    def toInt(b: Boolean) = if (b) 1 else 0
    (addrDelta << 2) | (toInt(hasStartDelta) << 1) | toInt(hasEndDelta)
  }

  def picklePositions(roots: List[Tree])(implicit ctx: Context) = {
    var lastIndex = 0
    var lastPos = Position(0, 0)
    def pickleDeltas(index: Int, pos: Position) = {
      val addrDelta = index - lastIndex
      val startDelta = pos.start - lastPos.start
      val endDelta = pos.end - lastPos.end
      buf.writeInt(header(addrDelta, startDelta != 0, endDelta != 0))
      if (startDelta != 0) buf.writeInt(startDelta)
      if (endDelta != 0) buf.writeInt(endDelta)
      lastIndex = index
      lastPos = pos
    }
    def traverse(x: Any): Unit = x match {
      case x: Tree @unchecked =>
        if (x.pos.exists /*&& x.pos.toSynthetic != x.initialPos.toSynthetic*/) {
          addrOfTree(x) match {
            case Some(addr) =>
              //println(i"pickling $x")
              pickleDeltas(addr.index, x.pos)
            case _ =>
              //println(i"no address for $x")
          }
        }
        //else println(i"skipping $x")
        x match {
          case x: MemberDef @unchecked => traverse(x.symbol.annotations)
          case _ =>
        }
        traverse(x.productIterator)
      case xs: TraversableOnce[_] =>
        xs.foreach(traverse)
      case _ =>
    }
    traverse(roots)
  }
}
