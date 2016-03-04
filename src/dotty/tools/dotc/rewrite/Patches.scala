package dotty.tools.dotc
package rewrite

import util.{SourceFile, Positions}
import Positions.Position
import core.Contexts.{Context, FreshContext}
import collection.mutable

object Patches {

  private case class Patch(pos: Position, replacement: String) {
    def delta = replacement.length - (pos.end - pos.start)
  }

  class PatchedFiles extends mutable.HashMap[SourceFile, Patches]

  /** If -rewrite is set, record a patch that replaces the range
   *  given by `pos` in `source` by `replacement`
   */
  def patch(source: SourceFile, pos: Position, replacement: String)(implicit ctx: Context): Unit =
    ctx.settings.rewrite.value match {
      case Some(pfs: PatchedFiles) =>
        pfs.get(source) match {
          case Some(ps) =>
            ps.addPatch(pos, replacement)
          case None =>
            pfs(source) = new Patches(source)
            patch(source, pos, replacement)
        }
      case _ =>
    }

  /** If -rewrite is set, apply all patches and overwrite patched source files.
   */
  def writeBack()(implicit ctx: Context) =
    ctx.settings.rewrite.value match {
      case Some(pfs: PatchedFiles) =>
        for (source <- pfs.keys) {
          ctx.println(s"[patched file ${source.file.path}]")
          pfs(source).writeBack()
        }
      case _ =>
    }
}

class Patches(source: SourceFile) {
  import Patches._

  private val pbuf = new mutable.ListBuffer[Patch]()

  def addPatch(pos: Position, replacement: String): Unit =
    pbuf += Patch(pos, replacement)

  def apply(cs: Array[Char]): Array[Char] = {
    val delta = pbuf.map(_.delta).sum
    val patches = pbuf.toList.sortBy(_.pos.start)
    patches.iterator.sliding(2, 1).foreach(ps =>
      assert(ps(0).pos.end <= ps(1).pos.start, s"overlapping patches: ${ps(0)} and ${ps(1)}"))
    val ds = new Array[Char](cs.length + delta)
    def loop(ps: List[Patch], inIdx: Int, outIdx: Int): Unit = {
      def copy(upTo: Int): Int = {
        val untouched = upTo - inIdx
        Array.copy(cs, inIdx, ds, outIdx, untouched)
        outIdx + untouched
      }
      ps match {
        case patch @ Patch(pos, replacement) :: ps1 =>
          val outNew = copy(pos.start)
          replacement.copyToArray(ds, outNew)
          loop(ps1, pos.end, outNew + replacement.length)
        case Nil =>
          val outNew = copy(cs.length)
          assert(outNew == ds.length, s"$outNew != ${ds.length}")
      }
    }
    loop(patches, 0, 0)
    ds
  }

  def writeBack(): Unit = {
    val out = source.file.output
    val chars = apply(source.content)
    val bytes = new String(chars).getBytes
    out.write(bytes)
    out.close()
  }
}