package dotty.tools.repl

import pprint.{Renderer, Result, Tree, Truncated}
import scala.util.matching.Regex

/** Wraps pprint with a minor fix for fansi encodings - TODO report upstream to get those fixed. 
  * https://github.com/com-lihaoyi/PPrint
  */
object PPrinter {
  // cached instance to avoid reinstantiation for each invocation
  private var pprinter: pprint.PPrinter | Null = null
  private var maxHeight: Int = Int.MaxValue
  private var nocolors: Boolean = false

  def apply(objectToRender: Object, maxHeight: Int = Int.MaxValue, nocolors: Boolean = false): String = {
    val _pprinter = this.synchronized {
      // initialise on first use and whenever the maxHeight setting changed
      if (pprinter == null || this.maxHeight != maxHeight || this.nocolors != nocolors) {
        this.pprinter = create(maxHeight, nocolors)
        this.maxHeight = maxHeight
        this.nocolors = nocolors
      }
      this.pprinter.nn
    }
    _pprinter.apply(objectToRender).render
  }

  private def create(maxHeight: Int, nocolors: Boolean): pprint.PPrinter = {
    val (colorLiteral, colorApplyPrefix) =
      if (nocolors) (fansi.Attrs.Empty, fansi.Attrs.Empty)
      else (fansi.Color.Green, fansi.Color.Yellow)

    new pprint.PPrinter(
      defaultHeight = maxHeight,
      colorLiteral = colorLiteral,
      colorApplyPrefix = colorApplyPrefix) {

      override def tokenize(x: Any,
                            width: Int = defaultWidth,
                            height: Int = defaultHeight,
                            indent: Int = defaultIndent,
                            initialOffset: Int = 0,
                            escapeUnicode: Boolean,
                            showFieldNames: Boolean): Iterator[fansi.Str] = {
        val tree = this.treeify(x, escapeUnicode = escapeUnicode, showFieldNames = showFieldNames)
        val renderer = new Renderer(width, this.colorApplyPrefix, this.colorLiteral, indent) {
          override def rec(x: Tree, leftOffset: Int, indentCount: Int): Result = x match {
            case Tree.Literal(body) if isAnsiEncoded(body) =>
              // this is the part we're overriding, everything else is just boilerplate
              Result.fromString(fixForFansi(body))
            case _ => super.rec(x, leftOffset, indentCount)
          }
        }
        val rendered = renderer.rec(tree, initialOffset, 0).iter
        new Truncated(rendered, width, height)
      }
    }
  }

  def isAnsiEncoded(string: String): Boolean =
    string.exists(c => c == '\u001b' || c == '\u009b')

  /** We use source-highlight to encode source as ansi strings, e.g. the .dump step Ammonite uses fansi for it's
    * colour-coding, and while both pledge to follow the ansi codec, they aren't compatible TODO: PR for fansi to
    * support these standard encodings out of the box
    */
  def fixForFansi(ansiEncoded: String): String = {
    import scala.language.unsafeNulls
    ansiEncoded
      .replaceAll("\u001b\\[m", "\u001b[39m")       // encoding ends with [39m for fansi instead of [m
      .replaceAll("\u001b\\[0(\\d)m", "\u001b[$1m") // `[01m` is encoded as `[1m` in fansi for all single digit numbers
      .replaceAll("\u001b\\[0?(\\d+);0?(\\d+)m", "\u001b[$1m\u001b[$2m") // `[01;34m` is encoded as `[1m[34m` in fansi
      .replaceAll(
        "\u001b\\[[00]+;0?(\\d+);0?(\\d+);0?(\\d+)m",
        "\u001b[$1;$2;$3m"
      ) // `[00;38;05;70m` is encoded as `[38;5;70m` in fansi - 8bit color encoding
  }

}
