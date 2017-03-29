package dotty.tools.dotc
package rewrite

import util.{SourceFile, Positions}
import Positions.Position
import core.Contexts.{Context, FreshContext}
import collection.mutable
import scala.annotation.tailrec

/** Handles rewriting of Scala2 files to Dotty */
object Rewrites {
  private class PatchedFiles extends mutable.HashMap[SourceFile, Patches]

  private case class Patch(pos: Position, replacement: String) {
    def delta = replacement.length - (pos.end - pos.start)
  }

  private class Patches(source: SourceFile) {
    private val pbuf = new mutable.ListBuffer[Patch]()

    def addPatch(pos: Position, replacement: String): Unit =
      pbuf += Patch(pos, replacement)

    def apply(cs: Array[Char]): Array[Char] = {
      val delta = pbuf.map(_.delta).sum
      val patches = pbuf.toList.sortBy(_.pos.start)
      if (patches.nonEmpty)
        patches reduceLeft {(p1, p2) =>
          assert(p1.pos.end <= p2.pos.start, s"overlapping patches: $p1 and $p2")
          p2
        }
      val ds = new Array[Char](cs.length + delta)
      @tailrec def loop(ps: List[Patch], inIdx: Int, outIdx: Int): Unit = {
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
      val chars = apply(source.underlying.content)
      val bytes = new String(chars).getBytes
      out.write(bytes)
      out.close()
    }
  }

  /** If -rewrite is set, record a patch that replaces the range
   *  given by `pos` in `source` by `replacement`
   */
  def patch(source: SourceFile, pos: Position, replacement: String)(implicit ctx: Context): Unit =
    for (rewrites <- ctx.settings.rewrite.value)
      rewrites.patched
        .getOrElseUpdate(source, new Patches(source))
        .addPatch(pos, replacement)

  /** Patch position in `ctx.compilationUnit.source`. */
  def patch(pos: Position, replacement: String)(implicit ctx: Context): Unit =
    patch(ctx.compilationUnit.source, pos, replacement)

  /** If -rewrite is set, apply all patches and overwrite patched source files.
   */
  def writeBack()(implicit ctx: Context) =
    for (rewrites <- ctx.settings.rewrite.value; source <- rewrites.patched.keys) {
      ctx.echo(s"[patched file ${source.file.path}]")
      rewrites.patched(source).writeBack()
    }
}

/** A completely encapsulated class representing rewrite state, used
 *  as an optional setting.
 */
class Rewrites {
  import Rewrites._
  private val patched = new PatchedFiles
}



