package dotty.tools.dotc.config

import java.lang.Character.isWhitespace
import java.nio.file.{Files, Paths}
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
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
   *  The embedded quotes are stripped. Escaping backslash is stripped.
   *
   *  Invoke `errorFn` with a descriptive message if an end quote is missing.
   */
  def tokenize(line: String, errorFn: String => Unit): List[String] =
    val accum = ListBuffer.empty[String]
    val buf   = new java.lang.StringBuilder
    var pos   = 0

    inline def cur    = if done then EOF else line.charAt(pos): Int
    inline def bump() = pos += 1
    inline def put()  = { buf.append(cur.toChar); bump() }
    inline def done   = pos >= line.length

    inline def skipWhitespace() = while isWhitespace(cur) do bump()

    // Collect to end of word, handling quotes. False for missing end quote.
    def word(): Boolean =
      var escaped = false
      var Q = EOF
      var lastQ = 0
      def badquote() = errorFn(s"Unmatched quote [${lastQ}](${line.charAt(lastQ)})")
      inline def inQuote = Q != EOF
      inline def finish(): Boolean = if (!inQuote) !escaped else { badquote(); false }
      @tailrec def advance(): Boolean = cur match
        case EOF                              => finish()
        case _ if escaped                     => escaped = false; put(); advance()
        case '\\'                             => escaped = true; bump(); advance()
        case q if q == Q                      => Q = EOF; bump(); advance()
        case q @ (DQ | SQ) if !inQuote        => Q = q; lastQ = pos; bump(); advance()
        case c if isWhitespace(c) && !inQuote => finish()
        case _                                => put(); advance()
      advance()

    // the current token, stripped of any embedded quotes.
    def text(): String =
      val res = buf.toString
      buf.setLength(0)
      res.nn

    @tailrec def loop(): List[String] =
      skipWhitespace()
      if done then
        accum.toList
      else if !word() then
        Nil
      else
        accum += text()
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
