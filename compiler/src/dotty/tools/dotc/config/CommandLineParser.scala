package dotty.tools.dotc.config

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import java.lang.Character.isWhitespace
import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters._

/** A simple enough command line parser.
 */
object CommandLineParser:
  inline private val DQ  = '"'
  inline private val SQ  = '\''
  inline private val EOF = -1

  /** Split the line into tokens separated by whitespace or quotes.
   *
   *  Invoke `errorFn` with message on bad quote.
   */
  def tokenize(line: String, errorFn: String => Unit): List[String] =

    var accum: List[String] = Nil

    var pos   = 0
    var start = 0
    val qpos  = new ArrayBuffer[Int](16)    // positions of paired quotes

    inline def cur    = if done then EOF else line.charAt(pos): Int
    inline def bump() = pos += 1
    inline def done   = pos >= line.length

    def skipToQuote(q: Int): Boolean =
      var escaped = false
      def terminal = cur match
        case _ if escaped => escaped = false ; false
        case '\\'         => escaped = true ; false
        case `q` | EOF    => true
        case _            => false
      while !terminal do bump()
      !done

    @tailrec def skipToDelim(): Boolean =
      inline def quote() = { qpos += pos ; bump() }
      cur match
        case q @ (DQ | SQ)        => { quote() ; skipToQuote(q) } && { quote() ; skipToDelim() }
        case -1                   => true
        case c if isWhitespace(c) => true
        case _                    => bump(); skipToDelim()

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

    def text(): String =
      val res =
        if qpos.isEmpty then line.substring(start, pos)
        else if qpos(0) == start && qpos(1) == pos then line.substring(start+1, pos-1)
        else copyText()
      qpos.clear()
      res.nn

    inline def badquote() = errorFn(s"Unmatched quote [${qpos.last}](${line.charAt(qpos.last)})")

    inline def skipWhitespace() = while isWhitespace(cur) do pos += 1

    @tailrec def loop(): List[String] =
      skipWhitespace()
      start = pos
      if done then
        accum.reverse
      else if !skipToDelim() then
        badquote()
        Nil
      else
        accum = text() :: accum
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
