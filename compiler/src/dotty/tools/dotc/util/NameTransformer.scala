package dotty.tools
package dotc
package util


import core.Names.*

import scala.annotation.internal.sharable

/** Provides functions to encode and decode Scala symbolic names.
 */
object NameTransformer {

  private val nops = 128
  private val ncodes = 26 * 26

  private class OpCodes(val op: Char, val code: String, val next: OpCodes)

  @sharable private val op2code = new Array[String](nops)
  @sharable private val code2op = new Array[OpCodes](ncodes)
  private def enterOp(op: Char, code: String) = {
    op2code(op.toInt) = code
    val c = (code.charAt(1) - 'a') * 26 + code.charAt(2) - 'a'
    code2op(c.toInt) = new OpCodes(op, code, code2op(c))
  }

  /* Note: decoding assumes opcodes are only ever lowercase. */
  enterOp('~', "$tilde")
  enterOp('=', "$eq")
  enterOp('<', "$less")
  enterOp('>', "$greater")
  enterOp('!', "$bang")
  enterOp('#', "$hash")
  enterOp('%', "$percent")
  enterOp('^', "$up")
  enterOp('&', "$amp")
  enterOp('|', "$bar")
  enterOp('*', "$times")
  enterOp('/', "$div")
  enterOp('+', "$plus")
  enterOp('-', "$minus")
  enterOp(':', "$colon")
  enterOp('\\', "$bslash")
  enterOp('?', "$qmark")
  enterOp('@', "$at")

  /** Replace operator symbols by corresponding expansion strings, and replace
   *  characters that are not valid Java identifiers by "$u" followed by the
   *  character's unicode expansion.
   *  Note that no attempt is made to escape the use of '$' in `name`: blindly
   *  escaping them might make it impossible to call some platform APIs. This
   *  unfortunately means that `decode(encode(name))` might not be equal to
   *  `name`, this is considered acceptable since '$' is a reserved character in
   *  the Scala spec as well as the Java spec.
   */
  def encode(name: SimpleName): SimpleName = {
    var buf: StringBuilder | Null = null
    val len = name.length
    var i = 0
    while (i < len) {
      val c = name(i)
      if (c < nops && (op2code(c.toInt) ne null)) {
        if (buf eq null) {
          buf = new StringBuilder()
          buf.append(name.sliceToString(0, i))
        }
        buf.append(op2code(c.toInt))
      /* Handle glyphs that are not valid Java/JVM identifiers */
      }
      else if (!Character.isJavaIdentifierPart(c)) {
        if (buf eq null) {
          buf = new StringBuilder()
          buf.append(name.sliceToString(0, i))
        }
        buf.append("$u%04X".format(c.toInt))
      }
      else if (buf ne null) {
        buf.append(c)
      }
      i += 1
    }
    if (buf eq null) name else termName(buf.toString)
  }

  /** Replace operator expansions by the operators themselves,
   *  and decode `$u....` expansions into unicode characters.
   */
  def decode(name: SimpleName): SimpleName = {
    //System.out.println("decode: " + name);//DEBUG
    var buf: StringBuilder | Null = null
    val len = name.length
    var i = 0
    while (i < len) {
      var ops: OpCodes | Null = null
      var unicode = false
      val c = name(i)
      if (c == '$' && i + 2 < len) {
        val ch1 = name(i + 1)
        if ('a' <= ch1 && ch1 <= 'z') {
          val ch2 = name(i + 2)
          if ('a' <= ch2 && ch2 <= 'z') {
            ops = code2op((ch1 - 'a') * 26 + ch2 - 'a')
            while ((ops ne null) && !name.startsWith(ops.code, i)) ops = ops.next
            if (ops ne null) {
              if (buf eq null) {
                buf = new StringBuilder()
                buf.append(name.sliceToString(0, i))
              }
              buf.append(ops.op)
              i += ops.code.length()
            }
            /* Handle the decoding of Unicode glyphs that are
             * not valid Java/JVM identifiers */
          } else if ((len - i) >= 6 && // Check that there are enough characters left
                     ch1 == 'u' &&
                     ((Character.isDigit(ch2)) ||
                     ('A' <= ch2 && ch2 <= 'F'))) {
            /* Skip past "$u", next four should be hexadecimal */
            val hex = name.sliceToString(i+2, i+6)
            try {
              val str = Integer.parseInt(hex, 16).toChar
              if (buf eq null) {
                buf = new StringBuilder()
                buf.append(name.sliceToString(0, i))
              }
              buf.append(str)
              /* 2 for "$u", 4 for hexadecimal number */
              i += 6
              unicode = true
            } catch {
              case _:NumberFormatException =>
                /* `hex` did not decode to a hexadecimal number, so
                 * do nothing. */
            }
          }
        }
      }
      /* If we didn't see an opcode or encoded Unicode glyph, and the
        buffer is non-empty, write the current character and advance
         one */
      if ((ops eq null) && !unicode) {
        if (buf ne null)
          buf.append(c)
        i += 1
      }
    }
    //System.out.println("= " + (if (buf == null) name else buf.toString()));//DEBUG
    if (buf eq null) name else termName(buf.toString)
  }
}
