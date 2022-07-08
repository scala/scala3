package dotty.tools.dotc
package rewrites

import util.{SourceFile, Spans}
import Spans.Span
import core.Contexts._
import collection.mutable
import scala.annotation.tailrec
import dotty.tools.dotc.reporting.Reporter

import java.nio.charset.StandardCharsets.UTF_8

/** Handles rewriting of Scala2 files to Dotty */
object Rewrites {
  private class PatchedFiles extends mutable.HashMap[SourceFile, Patches]

  private case class Patch(span: Span, replacement: String) {
    def delta = replacement.length - (span.end - span.start)
  }

  private class Patches(source: SourceFile) {
    private[Rewrites] val pbuf = new mutable.ListBuffer[Patch]()

    def addPatch(span: Span, replacement: String): Unit =
      pbuf.indexWhere(p => p.span.start == span.start && p.span.end == span.end) match {
        case i if i >= 0 => pbuf.update(i, Patch(span, replacement))
        case _           => pbuf += Patch(span, replacement)
      }

    def apply(cs: Array[Char]): Array[Char] = {
      val delta = pbuf.map(_.delta).sum
      val patches = pbuf.toList.sortBy(_.span.start)
      if (patches.nonEmpty)
        patches.reduceLeft {(p1, p2) =>
          assert(p1.span.end <= p2.span.start, s"overlapping patches in $source: $p1 and $p2")
          p2
        }
      val ds = new Array[Char](cs.length + delta)
      @tailrec def loop(ps: List[Patch], inIdx: Int, outIdx: Int): Unit = {
        def copy(upTo: Int): Int = {
          val untouched = upTo - inIdx
          System.arraycopy(cs, inIdx, ds, outIdx, untouched)
          outIdx + untouched
        }
        ps match {
          case patch @ Patch(span, replacement) :: ps1 =>
            val outNew = copy(span.start)
            replacement.copyToArray(ds, outNew)
            loop(ps1, span.end, outNew + replacement.length)
          case Nil =>
            val outNew = copy(cs.length)
            assert(outNew == ds.length, s"$outNew != ${ds.length}")
        }
      }
      loop(patches, 0, 0)
      ds
    }

    def writeBack(): Unit = {
      val chars = apply(source.underlying.content)
      val bytes = new String(chars).getBytes(UTF_8)
      val out = source.file.output
      out.write(bytes)
      out.close()
    }
  }

  /** If -rewrite is set, record a patch that replaces the range
   *  given by `span` in `source` by `replacement`
   */
  def patch(source: SourceFile, span: Span, replacement: String)(using Context): Unit =
    if ctx.reporter != Reporter.NoReporter // NoReporter is used for syntax highlighting
    then ctx.settings.rewrite.value.foreach(_.patched
         .getOrElseUpdate(source, new Patches(source))
         .addPatch(span, replacement)
    )

  /** Patch position in `ctx.compilationUnit.source`. */
  def patch(span: Span, replacement: String)(using Context): Unit =
    patch(ctx.compilationUnit.source, span, replacement)

  /** Does `span` overlap with a patch region of `source`? */
  def overlapsPatch(source: SourceFile, span: Span)(using Context): Boolean =
    ctx.settings.rewrite.value.exists(rewrites =>
      rewrites.patched.get(source).exists(patches =>
        patches.pbuf.exists(patch => patch.span.overlaps(span))))

  /** If -rewrite is set, apply all patches and overwrite patched source files.
   */
  def writeBack()(using Context): Unit =
    for (rewrites <- ctx.settings.rewrite.value; source <- rewrites.patched.keys) {
      report.echo(s"[patched file ${source.file.path}]")
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
