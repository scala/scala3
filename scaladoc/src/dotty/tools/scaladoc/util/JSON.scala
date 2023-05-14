package dotty.tools.scaladoc
package util

import scala.annotation.tailrec

opaque type JSON = String

def rawJSON(s: String): JSON = s

def jsonList(elems: Seq[JSON]): JSON = elems.mkString("[", ",\n", "]")

def jsonObject(fields: (String, JSON)*): JSON =
  fields.map{ case (k, v) => quoteStr(k)+":"+v}.mkString("{", ",", "}")


def quoteStr(str: String) = s""""$str""""

// based on Spray Json
def jsonString(s: String): JSON =
    def requiresEncoding(c: Char): Boolean =
      // from RFC 4627
      // unescaped = %x20-21 / %x23-5B / %x5D-10FFFF
      c match
        case '"'  => true
        case '\\' => true
        case c    => c < 0x20

    val sb = new StringBuilder
    @tailrec def firstToBeEncoded(ix: Int = 0): Int =
      if (ix == s.length) -1 else if (requiresEncoding(s.charAt(ix))) ix else firstToBeEncoded(ix + 1)

    sb.append('"')
    firstToBeEncoded() match
      case -1 => sb.append(s)
      case first =>
        // sb.append(s, 0, first) for "abc", 0, 2 produce "(abc,0,2)" rather then "ab" as in Java
        sb.append(s.substring(0, first))
        @tailrec def append(ix: Int): Unit =
          if (ix < s.length) {
            s.charAt(ix) match
              case c if !requiresEncoding(c) => sb.append(c)
              case '"' => sb.append("\\\"")
              case '\\' => sb.append("\\\\")
              case '\b' => sb.append("\\b")
              case '\f' => sb.append("\\f")
              case '\n' => sb.append("\\n")
              case '\r' => sb.append("\\r")
              case '\t' => sb.append("\\t")
              case x if x <= 0xF => sb.append("\\u000").append(Integer.toHexString(x))
              case x if x <= 0xFF => sb.append("\\u00").append(Integer.toHexString(x))
              case x if x <= 0xFFF => sb.append("\\u0").append(Integer.toHexString(x))
              case x => sb.append("\\u").append(Integer.toHexString(x))

            append(ix + 1)
          }
        append(first)

    sb.append('"').toString
