package dotty.tools.dotc.profile

import scala.annotation.internal.sharable

// Based on NameTransformer but dedicated for JSON encoding rules
object JsonNameTransformer {
  private val nops = 128

  @sharable private val op2code = new Array[String](nops)
  private def enterOp(op: Char, code: String) = op2code(op.toInt) = code

  enterOp('\"', "\\\"")
  enterOp('\\', "\\\\")
  // enterOp('/', "\\/") // optional, no need for escaping outside of html context
  enterOp('\b', "\\b")
  enterOp('\f', "\\f")
  enterOp('\n', "\\n")
  enterOp('\r', "\\r")
  enterOp('\t', "\\t")

  def encode(name: String): String = {
    var buf: StringBuilder = null.asInstanceOf
    val len = name.length
    var i = 0
    while (i < len) {
      val c = name(i)
      if (c < nops && (op2code(c.toInt) ne null)) {
        if (buf eq null) {
          buf = new StringBuilder()
          buf.append(name.subSequence(0, i))
        }
        buf.append(op2code(c.toInt))
      } else if (c <= 0x1F || c > 0x7F) {
        if (buf eq null) {
          buf = new StringBuilder()
          buf.append(name.subSequence(0, i))
        }
        buf.append("\\u%04X".format(c.toInt))
      } else if (buf ne null) {
        buf.append(c)
      }
      i += 1
    }
    if (buf eq null) name else buf.toString
  }
}