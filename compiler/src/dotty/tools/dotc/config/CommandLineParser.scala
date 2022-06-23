package dotty.tools.dotc.config

import java.lang.Character.isWhitespace
import java.nio.file.{Files, Paths}
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters.*

/** Split a line of text using shell conventions.
 */
object CommandLineParser:
  inline private val DQ  = '"'
  inline private val SQ  = '\''
  inline private val EOF = -1

  /** Split the line into tokens separated by whitespace.
   *
   *  Single or double quotes can be embedded to preserve internal whitespace:
   *
   *  `""" echo "hello, world!" """`   => "echo" :: "hello, world!" :: Nil
   *  `""" echo hello,' 'world! """`   => "echo" :: "hello, world!" :: Nil
   *  `""" echo \"hello, world!\" """` => "echo" :: "\"hello," :: "world!\"" :: Nil
   *
   *  The embedded quotes are stripped. Escaping backslash is not stripped.
   *
   *  Invoke `errorFn` with a descriptive message if an end quote is missing.
   */
  def tokenize(line: String, errorFn: String => Unit): List[String] =

    var accum: List[String] = Nil

    var pos   = 0
    var start = 0
    val qpos  = new ArrayBuffer[Int](16)    // positions of paired quotes in current token

    inline def cur    = if done then EOF else line.charAt(pos): Int
    inline def bump() = pos += 1
    inline def done   = pos >= line.length

    // Skip to the given unescaped end quote; false on no more input.
    def skipToEndQuote(q: Int): Boolean =
      var escaped = false
      def terminal = cur match
        case _ if escaped => escaped = false ; false
        case '\\'         => escaped = true ; false
        case `q` | EOF    => true
        case _            => false
      while !terminal do bump()
      !done

    // Skip to the next whitespace word boundary; record unescaped embedded quotes; false on missing quote.
    def skipToDelim(): Boolean =
      var escaped = false
      inline def quote() = { qpos += pos ; bump() }
      @tailrec def advance(): Boolean = cur match
        case _ if escaped         => escaped = false ; bump() ; advance()
        case '\\'                 => escaped = true ; bump() ; advance()
        case q @ (DQ | SQ)        => { quote() ; skipToEndQuote(q) } && { quote() ; advance() }
        case EOF                  => true
        case c if isWhitespace(c) => true
        case _                    => bump(); advance()
      advance()

    def copyText(): String =
      val buf = new java.lang.StringBuilder
      var p = start
      var i = 0
      while p < pos do
        if i >= qpos.size then
          buf.append(line, p, pos)
          p = pos
        else if p == qpos(i) then
          buf.append(line, qpos(i)+1, qpos(i+1))
          p = qpos(i+1)+1
          i += 2
        else
          buf.append(line, p, qpos(i))
          p = qpos(i)
      buf.toString

    // the current token, stripped of any embedded quotes.
    def text(): String =
      val res =
        if qpos.isEmpty then line.substring(start, pos)
        else if qpos(0) == start && qpos(1) == pos then line.substring(start+1, pos-1)
        else copyText()
      qpos.clear()
      res.nn

    inline def badquote() = errorFn(s"Unmatched quote [${qpos.last}](${line.charAt(qpos.last)})")

    inline def skipWhitespace() = while isWhitespace(cur) do bump()

    @tailrec def loop(): List[String] =
      skipWhitespace()
      start = pos
      if done then
        accum.reverse
      else if !skipToDelim() then
        badquote()
        Nil
      else
        accum ::= text()
        loop()
    end loop

    loop()
  end tokenize

  def tokenize(line: String): List[String] = tokenize(line, x => throw new ParseException(x))

  /** Expands all arguments starting with @ to the contents of the file named like each argument.
   */
  def expandArg(arg: String): List[String] =
    val path = Paths.get(arg.stripPrefix("@"))
    if !Files.exists(path) then
      System.err.nn.println(s"Argument file ${path.nn.getFileName} could not be found")
      Nil
    else
      def stripComment(s: String) = s.indexOf('#') match { case -1 => s case i => s.substring(0, i) }
      val lines = Files.readAllLines(path).nn
      val params = lines.asScala.map(stripComment).filter(!_.nn.isEmpty).mkString(" ")
      tokenize(params)

  class ParseException(msg: String) extends RuntimeException(msg)
