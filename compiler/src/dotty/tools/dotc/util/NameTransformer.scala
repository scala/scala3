package dotty.tools
package dotc
package util

import core.Names._
import collection.mutable
import scala.internal.Chars

import scala.annotation.internal.sharable

/** Provides functions to encode and decode Scala symbolic names.
 */
object NameTransformer {

  private val nops = 128

  @sharable private val op2code = new Array[String](nops)
  @sharable private val str2op = new mutable.HashMap[String, Char]

  private def enterOp(op: Char, code: String) = {
    op2code(op) = code
    str2op(code) = op
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

  /** Expand characters that are illegal as JVM method names by `$u`, followed
   *  by the character's unicode expansion.
   */
  def avoidIllegalChars(name: SimpleName): SimpleName = {
    var i = name.length - 1
    while (i >= 0 && Chars.isValidJVMMethodChar(name(i))) i -= 1
    if (i >= 0)
      termName(
        name.toString.flatMap(ch =>
          if (Chars.isValidJVMMethodChar(ch)) ch.toString else "$u%04X".format(ch.toInt)))
    else name
  }

  /** Decode expanded characters starting with `$u`, followed by the character's unicode expansion. */
  def decodeIllegalChars(name: String): String = {
    if (name.contains("$u")) {
      val sb = new mutable.StringBuilder()
      var i = 0
      while (i < name.length) {
        if (i < name.length - 5 && name(i) == '$' && name(i + 1) == 'u') {
          val numbers = name.substring(i + 2, i + 6)
          try sb.append(Integer.valueOf(name.substring(i + 2, i + 6), 16).toChar)
          catch {
            case _: java.lang.NumberFormatException =>
              sb.append("$u").append(numbers)
          }
          i += 6
        } else {
          sb.append(name(i))
          i += 1
        }
      }
      sb.result()
    }
    else name
  }

  /** Replace operator symbols by corresponding expansion strings.
   *
   *  @param name the string to encode
   *  @return     the string with all recognized opchars replaced with their encoding
   *
   *  Operator symbols are only recognized if they make up the whole name, or
   *  if they make up the last part of the name which follows a `_`.
   */
  def encode(name: SimpleName): SimpleName = {
    def loop(len: Int, ops: List[String]): SimpleName = {
      def convert =
        if (ops.isEmpty) name
        else {
          val buf = new java.lang.StringBuilder
          buf.append(chrs, name.start, len)
          for (op <- ops) buf.append(op)
          termName(buf.toString)
        }
      if (len == 0 || name(len - 1) == '_') convert
      else {
        val ch = name(len - 1)
        if (ch <= nops && op2code(ch) != null)
          loop(len - 1, op2code(ch) :: ops)
        else if (Chars.isSpecial(ch))
          loop(len - 1, ch.toString :: ops)
        else name
      }
    }
    loop(name.length, Nil)
  }

  /** Replace operator expansions by the operators themselves.
   *  Operator expansions are only recognized if they make up the whole name, or
   *  if they make up the last part of the name which follows a `_`.
   */
  def decode(name: SimpleName): SimpleName = {
    def loop(len: Int, ops: List[Char]): SimpleName = {
      def convert =
        if (ops.isEmpty) name
        else {
          val buf = new java.lang.StringBuilder
          buf.append(chrs, name.start, len)
          for (op <- ops) buf.append(op)
          termName(buf.toString)
        }
      if (len == 0 || name(len - 1) == '_') convert
      else if (Chars.isSpecial(name(len - 1))) loop(len - 1, name(len - 1) :: ops)
      else {
        val idx = name.lastIndexOf('$', len - 1)
        if (idx >= 0 && idx + 2 < len)
          str2op.get(name.sliceToString(idx, len)) match {
            case Some(ch) => loop(idx, ch :: ops)
            case None => name
          }
        else name
      }
    }
    loop(name.length, Nil)
  }
}
