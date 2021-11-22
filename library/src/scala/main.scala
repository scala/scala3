/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala

import collection.mutable
import annotation.MainAnnotation

/** An annotation that designates a main function
 */
final class main(maxLineLength: Int) extends MainAnnotation:
  self =>
  import main._

  def this() = this(120)

  override type ArgumentParser[T] = util.CommandLineParser.FromString[T]
  override type MainResultType = Any

  private enum ArgumentKind {
    case SimpleArgument, OptionalArgument, VarArgument
  }

  override def command(args: Array[String], commandName: String, docComment: String) =
    new MainAnnotation.Command[ArgumentParser, MainResultType]:
      private var argNames = new mutable.ArrayBuffer[String]
      private var argTypes = new mutable.ArrayBuffer[String]
      private var argKinds = new mutable.ArrayBuffer[ArgumentKind]

      private val documentation = new main.Documentation {
        /*
        * Most of this class' helper methods come from {{dotty.tools.dotc.util.Chars}}
        * and {{dotty.tools.dotc.util.CommentParsing}}.
        */

        /** The main part of the documentation. */
        def mainDoc: String = _mainDoc
        /** The parameters identified by @param. Maps from param name to documentation. */
        def argDocs: Map[String, String] = _argDocs

        private var _mainDoc: String = ""
        private var _argDocs: Map[String, String] = Map().withDefaultValue("")

        docComment.trim match {
          case "" =>
          case doc => parseDocComment(doc.nn)
        }

        private def isWhitespace(c: Char): Boolean =
          c == ' ' || c == '\t' || c == '\u000D'

        private def isIdentifierPart(c: Char): Boolean =
          (c == '$') || java.lang.Character.isUnicodeIdentifierPart(c)

        private def skipWhitespace(str: String, start: Int): Int =
          if (start < str.length && isWhitespace(str charAt start)) skipWhitespace(str, start + 1)
          else start

        private def skipIdent(str: String, start: Int): Int =
          if (start < str.length && isIdentifierPart(str charAt start)) skipIdent(str, start + 1)
          else start

        private def skipTag(str: String, start: Int): Int =
          if (start < str.length && (str charAt start) == '@') skipIdent(str, start + 1)
          else start

        private def skipLineLead(str: String, start: Int): Int =
          if (start == str.length) start
          else {
            val idx = skipWhitespace(str, start + 1)
            if (idx < str.length && (str charAt idx) == '*') skipWhitespace(str, idx + 1)
            else if (idx + 2 < str.length && (str charAt idx) == '/' && (str charAt (idx + 1)) == '*' && (str charAt (idx + 2)) == '*')
              skipWhitespace(str, idx + 3)
            else idx
          }

        private def startTag(str: String, sections: List[(Int, Int)]): Int = sections match {
          case Nil             => str.length - 2
          case (start, _) :: _ => start
        }

        private def skipToEol(str: String, start: Int): Int =
          if (start + 2 < str.length && (str charAt start) == '/' && (str charAt (start + 1)) == '*' && (str charAt (start + 2)) == '*') start + 3
          else if (start < str.length && (str charAt start) != '\n') skipToEol(str, start + 1)
          else start

        private def findNext(str: String, start: Int)(p: Int => Boolean): Int = {
          val idx = skipLineLead(str, skipToEol(str, start))
          if (idx < str.length && !p(idx)) findNext(str, idx)(p)
          else idx
        }

        private def findAll(str: String, start: Int)(p: Int => Boolean): List[Int] = {
          val idx = findNext(str, start)(p)
          if (idx == str.length) List()
          else idx :: findAll(str, idx)(p)
        }

        private def tagIndex(str: String, p: Int => Boolean = (idx => true)): List[(Int, Int)] = {
          var indices = findAll(str, 0) (idx => str(idx) == '@' && p(idx))
          indices = mergeUsecaseSections(str, indices)
          indices = mergeInheritdocSections(str, indices)

          indices match {
            case List() => List()
            case idxs   => idxs zip (idxs.tail ::: List(str.length - 2))
          }
        }

        private def mergeUsecaseSections(str: String, idxs: List[Int]): List[Int] =
          idxs.indexWhere(str.startsWith("@usecase", _)) match {
            case firstUCIndex if firstUCIndex != -1 =>
              val commentSections = idxs.take(firstUCIndex)
              val usecaseSections = idxs.drop(firstUCIndex).filter(str.startsWith("@usecase", _))
              commentSections ::: usecaseSections
            case _ =>
              idxs
          }

        private def mergeInheritdocSections(str: String, idxs: List[Int]): List[Int] =
          idxs.filterNot(str.startsWith("@inheritdoc", _))

        private def startsWithTag(str: String, section: (Int, Int), tag: String): Boolean =
          startsWithTag(str, section._1, tag)

        private def startsWithTag(str: String, start: Int, tag: String): Boolean =
          str.startsWith(tag, start) && !isIdentifierPart(str charAt (start + tag.length))

        private def paramDocs(str: String, tag: String, sections: List[(Int, Int)]): Map[String, (Int, Int)] =
          Map() ++ {
            for (section <- sections if startsWithTag(str, section, tag)) yield {
              val start = skipWhitespace(str, section._1 + tag.length)
              str.substring(start, skipIdent(str, start)).nn -> section
            }
          }

        private def extractSectionText(str: String, section: (Int, Int)): (Int, Int) = {
          val (beg, end) = section
          if (str.startsWith("@param", beg) ||
              str.startsWith("@tparam", beg) ||
              str.startsWith("@throws", beg))
            (skipWhitespace(str, skipIdent(str, skipWhitespace(str, skipTag(str, beg)))), end)
          else
            (skipWhitespace(str, skipTag(str, beg)), end)
        }

        private def format(raw: String): String =
          val remove = List("/**", "*", "/*", "*/")
          var lines: Seq[String] = raw.trim.nn.split('\n').toSeq
          lines = lines.map(l => l.substring(skipLineLead(l, -1), l.length).nn.trim.nn)
          var s = lines.foldLeft("") {
            case ("", s2) => s2
            case (s1, "") if s1.last == '\n' => s1 // Multiple newlines are kept as single newlines
            case (s1, "") => s1 + '\n'
            case (s1, s2) if s1.last == '\n' => s1 + s2
            case (s1, s2) => s1 + ' ' + s2
          }
          s.replaceAll(raw"\{\{", "").nn.replaceAll(raw"\}\}", "").nn.trim.nn

        private def parseDocComment(trimmedDoc: String): Unit =
          // Positions of the sections (@) in the docstring
          val tidx: List[(Int, Int)] = tagIndex(trimmedDoc)

          // Parse main comment
          var mainComment: String = trimmedDoc.substring(skipLineLead(trimmedDoc, 0), startTag(trimmedDoc, tidx)).nn
          _mainDoc = format(mainComment)

          // Parse arguments comments
          val argsCommentsSpans: Map[String, (Int, Int)] = paramDocs(trimmedDoc, "@param", tidx)
          val argsCommentsTextSpans = argsCommentsSpans.view.mapValues(extractSectionText(trimmedDoc, _))
          val argsCommentsTexts = argsCommentsTextSpans.mapValues { case (beg, end) => trimmedDoc.substring(beg, end) }
          _argDocs = argsCommentsTexts.mapValues(s => format(s.nn)).toMap.withDefaultValue("")
      }

      /** A buffer for all errors */
      private var errors = new mutable.ArrayBuffer[String]

      /** Issue an error, and return an uncallable getter */
      private def error(msg: String): () => Nothing =
        errors += msg
        () => throw new AssertionError("trying to get invalid argument")

      /** The next argument index */
      private var argIdx: Int = 0

      private def argAt(idx: Int): Option[String] =
        if idx < args.length then Some(args(idx)) else None

      private def nextPositionalArg(): Option[String] =
        while argIdx < args.length && args(argIdx).startsWith("--") do argIdx += 2
        val result = argAt(argIdx)
        argIdx += 1
        result

      private def convert[T](argName: String, arg: String, p: ArgumentParser[T]): () => T =
        p.fromStringOption(arg) match
          case Some(t) => () => t
          case None => error(s"invalid argument for $argName: $arg")

      private def argUsage(pos: Int): String =
        val name = argNames(pos)

        argKinds(pos) match {
          case ArgumentKind.SimpleArgument => s"[--$name] <${argTypes(pos)}>"
          case ArgumentKind.OptionalArgument => s"[[--$name] <${argTypes(pos)}>]"
          case ArgumentKind.VarArgument => s"[<${argTypes(pos)}> [<${argTypes(pos)}> [...]]]"
        }

      def wrapLongLine(line: String, maxLength: Int): List[String] = {
        def recurse(s: String, acc: Vector[String]): Seq[String] =
          val lastSpace = s.trim.nn.lastIndexOf(' ', maxLength)
          if ((s.length <= maxLength) || (lastSpace < 0))
            acc :+ s
          else {
            val (shortLine, rest) = s.splitAt(lastSpace)
            recurse(rest.trim.nn, acc :+ shortLine)
          }

        recurse(line, Vector()).toList
      }

      def wrapArgumentUsages(argsUsage: List[String], maxLength: Int): List[String] = {
        def recurse(args: List[String], currentLine: String, acc: Vector[String]): Seq[String] =
          (args, currentLine) match {
            case (Nil, "") => acc
            case (Nil, l) => (acc :+ l)
            case (arg :: t, "") => recurse(t, arg, acc)
            case (arg :: t, l) if l.length + 1 + arg.length <= maxLength => recurse(t, s"$l $arg", acc)
            case (arg :: t, l) => recurse(t, arg, acc :+ l)
          }

        recurse(argsUsage, "", Vector()).toList
      }

      private def usage(): Unit =
        val usageBeginning = s"Usage: $commandName "
        val argsOffset = usageBeginning.length
        val argUsages = wrapArgumentUsages((0 until argNames.length).map(argUsage).toList, maxLineLength - argsOffset)

        println(usageBeginning + argUsages.mkString("\n" + " " * argsOffset))

      private def explain(): Unit =
        if (documentation.mainDoc.nonEmpty)
          println(wrapLongLine(documentation.mainDoc, maxLineLength).mkString("\n"))
        if (argNames.nonEmpty) {
          val argNameShift = 2
          val argDocShift = argNameShift + 2

          println("Arguments:")
          for (pos <- 0 until argNames.length)
            val argDocBuilder = StringBuilder(" " * argNameShift)
            argDocBuilder.append(s"${argNames(pos)} - ${argTypes(pos)}")

            argKinds(pos) match {
              case ArgumentKind.OptionalArgument => argDocBuilder.append(" (optional)")
              case ArgumentKind.VarArgument => argDocBuilder.append(" (vararg)")
              case _ =>
            }

            val argDoc = documentation.argDocs(argNames(pos))
            if (argDoc.nonEmpty) {
              val formatedDocLines =
                argDoc
                .split("\n").nn
                .map(line => wrapLongLine(line.nn, maxLineLength - argDocShift).map(" " * argDocShift + _).mkString("\n"))
              argDocBuilder.append("\n").append(formatedDocLines.mkString("\n"))
            }

            println(argDocBuilder)
        }

      private def indicesOfArg(argName: String): Seq[Int] =
        def allIndicesOf(s: String): Seq[Int] =
          def recurse(s: String, from: Int): Seq[Int] =
            val i = args.indexOf(s, from)
            if i < 0 then Seq() else i +: recurse(s, i + 1)

          recurse(s, 0)

        val indices = allIndicesOf(s"--$argName")
        indices.filter(_ >= 0)

      private def getArgGetter[T](argName: String, getDefaultGetter: () => () => T)(using p: ArgumentParser[T]): () => T =
        indicesOfArg(argName) match {
          case s @ (Seq() | Seq(_)) =>
            val argOpt = s.headOption.map(idx => argAt(idx + 1)).getOrElse(nextPositionalArg())
            argOpt match {
              case Some(arg) => convert(argName, arg, p)
              case None => getDefaultGetter()
            }
          case s =>
            val multValues = s.flatMap(idx => argAt(idx + 1))
            error(s"more than one value for $argName: ${multValues.mkString(", ")}")
        }

      private def registerArg(argName: String, argType: String, argKind: ArgumentKind): Unit =
        argNames += argName
        argTypes += argType
        argKinds += argKind

      override def argGetter[T](argName: String, argType: String)(using p: ArgumentParser[T]): () => T =
        registerArg(argName, argType, ArgumentKind.SimpleArgument)
        getArgGetter(argName, () => error(s"missing argument for $argName"))

      override def argGetterDefault[T](argName: String, argType: String, defaultValue: => T)(using p: ArgumentParser[T]): () => T =
        registerArg(argName, argType, ArgumentKind.OptionalArgument)
        getArgGetter(argName, () => () => defaultValue)

      override def argsGetter[T](argName: String, argType: String)(using p: ArgumentParser[T]): () => Seq[T] =
        registerArg(argName, argType, ArgumentKind.VarArgument)
        def remainingArgGetters(): List[() => T] = nextPositionalArg() match
          case Some(arg) => convert(argName, arg, p) :: remainingArgGetters()
          case None => Nil
        val getters = remainingArgGetters()
        () => getters.map(_())

      override def run(f: => MainResultType): Unit =
        def flagUnused(): Unit = nextPositionalArg() match
          case Some(arg) =>
            error(s"unused argument: $arg")
            flagUnused()
          case None =>
            for
              arg <- args
              if arg.startsWith("--") && !argNames.contains(arg.drop(2))
            do
              error(s"unknown argument name: $arg")
        end flagUnused

        if args.contains("--help") then
          usage()
          println()
          explain()
        else
          flagUnused()
          if errors.nonEmpty then
            for msg <- errors do println(s"Error: $msg")
            usage()
          else f match
            case ExitCode(n) => sys.exit(n)
            case () =>
            case res =>
              val wrappedLines = res.toString.split("\n").nn.map(line => wrapLongLine(line.nn, maxLineLength).mkString("\n"))
              println(wrappedLines.mkString("\n"))
      end run
  end command
end main

object main:
  case class ExitCode(code: Int)

  private trait Documentation:
    /** The main part of the documentation. */
    def mainDoc: String
    /** The parameters identified by @param. Maps from param name to documentation. */
    def argDocs: Map[String, String]
end main
