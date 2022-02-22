package dotty.tools
package dotc
package core

import ast.{ untpd, tpd }
import Symbols._, Contexts._
import util.{SourceFile, ReadOnlyMap}
import util.Spans._
import util.CommentParsing._
import util.Property.Key
import parsing.Parsers.Parser
import reporting.ProperDefinitionNotFound

object Comments {
  val ContextDoc: Key[ContextDocstrings] = new Key[ContextDocstrings]

  /** Decorator for getting docbase out of context */
  given CommentsContext: AnyRef with
    extension (c: Context) def docCtx: Option[ContextDocstrings] = c.property(ContextDoc)

  /** Context for Docstrings, contains basic functionality for getting
    * docstrings via `Symbol` and expanding templates
    */
  class ContextDocstrings {

    private val _docstrings: MutableSymbolMap[Comment] = MutableSymbolMap[Comment](512) // FIXME: 2nd [Comment] needed or "not a class type"

    val templateExpander: CommentExpander = new CommentExpander

    def docstrings: ReadOnlyMap[Symbol, Comment] = _docstrings

    def docstring(sym: Symbol): Option[Comment] = _docstrings.get(sym)

    def addDocstring(sym: Symbol, doc: Option[Comment]): Unit =
      doc.foreach(d => _docstrings.update(sym, d))
  }

  /**
   * A `Comment` contains the unformatted docstring, it's position and potentially more
   * information that is populated when the comment is "cooked".
   *
   * @param span     The position span of this `Comment`.
   * @param raw      The raw comment, as seen in the source code, without any expansion.
   * @param expanded If this comment has been expanded, it's expansion, otherwise `None`.
   * @param usecases The usecases for this comment.
   */
  final case class Comment(
    span: Span,
    raw: String,
    expanded: Option[String],
    usecases: List[UseCase],
    variables: Map[String, String],
  ) {

    /** Has this comment been cooked or expanded? */
    def isExpanded: Boolean = expanded.isDefined

    /** The body of this comment, without the `@usecase` and `@define` sections, after expansion. */
    lazy val expandedBody: Option[String] =
      expanded.map(removeSections(_, "@usecase", "@define"))

    val isDocComment: Boolean = Comment.isDocComment(raw)

    /**
     * Expands this comment by giving its content to `f`, and then parsing the `@usecase` sections.
     * Typically, `f` will take care of expanding the variables.
     *
     * @param f The expansion function.
     * @return The expanded comment, with the `usecases` populated.
     */
    def expand(f: String => String)(using Context): Comment = {
      val expandedComment = f(raw)
      val useCases = Comment.parseUsecases(expandedComment, span)
      Comment(span, raw, Some(expandedComment), useCases, Map.empty)
    }
  }

  object Comment {

    def isDocComment(comment: String): Boolean = comment.startsWith("/**")

    def apply(span: Span, raw: String): Comment =
      Comment(span, raw, None, Nil, Map.empty)

    private def parseUsecases(expandedComment: String, span: Span)(using Context): List[UseCase] =
      if (!isDocComment(expandedComment))
        Nil
      else
        tagIndex(expandedComment)
          .filter { startsWithTag(expandedComment, _, "@usecase") }
          .map { case (start, end) => decomposeUseCase(expandedComment, span, start, end) }

    /** Turns a usecase section into a UseCase, with code changed to:
     *  {{{
     *  // From:
     *  def foo: A
     *  // To:
     *  def foo: A = ???
     *  }}}
     */
    private def decomposeUseCase(body: String, span: Span, start: Int, end: Int)(using Context): UseCase = {
      def subPos(start: Int, end: Int) =
        if (span == NoSpan) NoSpan
        else {
          val start1 = span.start + start
          val end1 = span.end + end
          span withStart start1 withPoint start1 withEnd end1
        }

      val codeStart = skipWhitespace(body, start + "@usecase".length)
      val codeEnd   = skipToEol(body, codeStart)
      val code      = body.substring(codeStart, codeEnd) + " = ???"
      val codePos   = subPos(codeStart, codeEnd)

      UseCase(code, codePos)
    }
  }

  final case class UseCase(code: String, codePos: Span, untpdCode: untpd.Tree, tpdCode: Option[tpd.DefDef]) {
    def typed(tpdCode: tpd.DefDef): UseCase = copy(tpdCode = Some(tpdCode))
  }

  object UseCase {
    def apply(code: String, codePos: Span)(using Context): UseCase = {
      val tree = {
        val tree = new Parser(SourceFile.virtual("<usecase>", code)).localDef(codePos.start)
        tree match {
          case tree: untpd.DefDef =>
            val newName = ctx.compilationUnit.freshNames.newName(tree.name, NameKinds.DocArtifactName)
            untpd.cpy.DefDef(tree)(name = newName)
          case _ =>
            report.error(ProperDefinitionNotFound(), ctx.source.atSpan(codePos))
            tree
        }
      }
      UseCase(code, codePos, tree, None)
    }
  }

  /**
   * Port of DocComment.scala from nsc
   * @author Martin Odersky
   * @author Felix Mulder
   */
  class CommentExpander {
    import dotc.config.Printers.scaladoc
    import scala.collection.mutable

    def expand(sym: Symbol, site: Symbol)(using Context): String = {
      val parent = if (site != NoSymbol) site else sym
      defineVariables(parent)
      expandedDocComment(sym, parent)
    }

    /** The cooked doc comment of symbol `sym` after variable expansion, or "" if missing.
     *
     *  @param sym  The symbol for which doc comment is returned
     *  @param site The class for which doc comments are generated
     *  @throws ExpansionLimitExceeded  when more than 10 successive expansions
     *                                  of the same string are done, which is
     *                                  interpreted as a recursive variable definition.
     */
    def expandedDocComment(sym: Symbol, site: Symbol, docStr: String = "")(using Context): String = {
      // when parsing a top level class or module, use the (module-)class itself to look up variable definitions
      val parent = if ((sym.is(Flags.Module) || sym.isClass) && site.is(Flags.Package)) sym
                   else site
      expandVariables(cookedDocComment(sym, docStr), sym, parent)
    }

    private def template(raw: String): String =
      removeSections(raw, "@define")

    private def defines(raw: String): List[String] = {
      val sections = tagIndex(raw)
      val defines = sections filter { startsWithTag(raw, _, "@define") }
      val usecases = sections filter { startsWithTag(raw, _, "@usecase") }
      val end = startTag(raw, (defines ::: usecases).sortBy(_._1))

      defines map { case (start, end) => raw.substring(start, end) }
    }

    private def replaceInheritDocToInheritdoc(docStr: String): String  =
      docStr.replaceAll("""\{@inheritDoc\p{Zs}*\}""", "@inheritdoc")

    /** The cooked doc comment of an overridden symbol */
    protected def superComment(sym: Symbol)(using Context): Option[String] =
      allInheritedOverriddenSymbols(sym).iterator map (x => cookedDocComment(x)) find (_ != "")

    private val cookedDocComments = MutableSymbolMap[String]()

    /** The raw doc comment of symbol `sym`, minus usecase and define sections, augmented by
     *  missing sections of an inherited doc comment.
     *  If a symbol does not have a doc comment but some overridden version of it does,
     *  the doc comment of the overridden version is copied instead.
     */
    def cookedDocComment(sym: Symbol, docStr: String = "")(using Context): String = cookedDocComments.getOrElseUpdate(sym, {
      var ownComment =
        if (docStr.length == 0) ctx.docCtx.flatMap(_.docstring(sym).map(c => template(c.raw))).getOrElse("")
        else template(docStr)
      ownComment = replaceInheritDocToInheritdoc(ownComment)

      superComment(sym) match {
        case None =>
          // SI-8210 - The warning would be false negative when this symbol is a setter
          if (ownComment.indexOf("@inheritdoc") != -1 && ! sym.isSetter)
            scaladoc.println(s"${sym.span}: the comment for ${sym} contains @inheritdoc, but no parent comment is available to inherit from.")
          ownComment.replace("@inheritdoc", "<invalid inheritdoc annotation>")
        case Some(sc) =>
          if (ownComment == "") sc
          else expandInheritdoc(sc, merge(sc, ownComment, sym), sym)
      }
    })

    private def isMovable(str: String, sec: (Int, Int)): Boolean =
      startsWithTag(str, sec, "@param") ||
      startsWithTag(str, sec, "@tparam") ||
      startsWithTag(str, sec, "@return")

    def merge(src: String, dst: String, sym: Symbol, copyFirstPara: Boolean = false): String = {
      val srcSections  = tagIndex(src)
      val dstSections  = tagIndex(dst)
      val srcParams    = paramDocs(src, "@param", srcSections)
      val dstParams    = paramDocs(dst, "@param", dstSections)
      val srcTParams   = paramDocs(src, "@tparam", srcSections)
      val dstTParams   = paramDocs(dst, "@tparam", dstSections)
      val out          = new StringBuilder
      var copied       = 0
      var tocopy       = startTag(dst, dstSections dropWhile (!isMovable(dst, _)))

      if (copyFirstPara) {
        val eop = // end of comment body (first para), which is delimited by blank line, or tag, or end of comment
          (findNext(src, 0)(src.charAt(_) == '\n')) min startTag(src, srcSections)
        out append src.substring(0, eop).trim
        copied = 3
        tocopy = 3
      }

      def mergeSection(srcSec: Option[(Int, Int)], dstSec: Option[(Int, Int)]) = dstSec match {
        case Some((start, end)) =>
          if (end > tocopy) tocopy = end
        case None =>
          srcSec match {
            case Some((start1, end1)) =>
              out append dst.substring(copied, tocopy).trim
              out append "\n"
              copied = tocopy
              out append src.substring(start1, end1).trim
            case None =>
          }
      }

      //TODO: enable this once you know how to get `sym.paramss`
      /*
      for (params <- sym.paramss; param <- params)
        mergeSection(srcParams get param.name.toString, dstParams get param.name.toString)
      for (tparam <- sym.typeParams)
        mergeSection(srcTParams get tparam.name.toString, dstTParams get tparam.name.toString)

      mergeSection(returnDoc(src, srcSections), returnDoc(dst, dstSections))
      mergeSection(groupDoc(src, srcSections), groupDoc(dst, dstSections))
      */

      if (out.length == 0) dst
      else {
        out append dst.substring(copied)
        out.toString
      }
    }

    /**
     * Expand inheritdoc tags
     *  - for the main comment we transform the inheritdoc into the super variable,
     *  and the variable expansion can expand it further
     *  - for the param, tparam and throws sections we must replace comments on the spot
     *
     * This is done separately, for two reasons:
     * 1. It takes longer to run compared to merge
     * 2. The inheritdoc annotation should not be used very often, as building the comment from pieces severely
     * impacts performance
     *
     * @param parent The source (or parent) comment
     * @param child  The child (overriding member or usecase) comment
     * @param sym    The child symbol
     * @return       The child comment with the inheritdoc sections expanded
     */
    def expandInheritdoc(parent: String, child: String, sym: Symbol): String =
      if (child.indexOf("@inheritdoc") == -1)
        child
      else {
        val parentSections    = tagIndex(parent)
        val childSections     = tagIndex(child)
        val parentTagMap      = sectionTagMap(parent, parentSections)
        val parentNamedParams = Map() +
          ("@param"  -> paramDocs(parent, "@param", parentSections)) +
          ("@tparam" -> paramDocs(parent, "@tparam", parentSections)) +
          ("@throws" -> paramDocs(parent, "@throws", parentSections))

        val out         = new StringBuilder

        def replaceInheritdoc(childSection: String, parentSection: => String) =
          if (childSection.indexOf("@inheritdoc") == -1)
            childSection
          else
            childSection.replace("@inheritdoc", parentSection)

        def getParentSection(section: (Int, Int)): String = {

          def getSectionHeader = extractSectionTag(child, section) match {
            case param@("@param"|"@tparam"|"@throws")  => param + " "  + extractSectionParam(child, section)
            case other     => other
          }

          def sectionString(param: String, paramMap: Map[String, (Int, Int)]): String =
            paramMap.get(param) match {
              case Some(section) =>
                // Cleanup the section tag and parameter
                val sectionTextBounds = extractSectionText(parent, section)
                cleanupSectionText(parent.substring(sectionTextBounds._1, sectionTextBounds._2))
              case None =>
                scaladoc.println(s"""${sym.span}: the """" + getSectionHeader + "\" annotation of the " + sym +
                    " comment contains @inheritdoc, but the corresponding section in the parent is not defined.")
                "<invalid inheritdoc annotation>"
            }

          child.substring(section._1, section._1 + 7) match {
            case param@("@param "|"@tparam"|"@throws") =>
              sectionString(extractSectionParam(child, section), parentNamedParams(param.trim))
            case _                                     =>
              sectionString(extractSectionTag(child, section), parentTagMap)
          }
        }

        def mainComment(str: String, sections: List[(Int, Int)]): String =
          if (str.trim.length > 3)
            str.trim.substring(3, startTag(str, sections))
          else
            ""

        // Append main comment
        out.append("/**")
        out.append(replaceInheritdoc(mainComment(child, childSections), mainComment(parent, parentSections)))

        // Append sections
        for (section <- childSections)
          out.append(replaceInheritdoc(child.substring(section._1, section._2), getParentSection(section)))

        out.append("*/")
        out.toString
      }

    protected def expandVariables(initialStr: String, sym: Symbol, site: Symbol)(using Context): String = {
      val expandLimit = 10

      def expandInternal(str: String, depth: Int): String = {
        if (depth >= expandLimit)
          throw new ExpansionLimitExceeded(str)

        val out         = new StringBuilder
        var copied, idx = 0
        // excluding variables written as \$foo so we can use them when
        // necessary to document things like Symbol#decode
        def isEscaped = idx > 0 && str.charAt(idx - 1) == '\\'
        while (idx < str.length)
          if ((str charAt idx) != '$' || isEscaped)
            idx += 1
          else {
            val vstart = idx
            idx = skipVariable(str, idx + 1)
            def replaceWith(repl: String) = {
              out append str.substring(copied, vstart)
              out append repl
              copied = idx
            }
            variableName(str.substring(vstart + 1, idx)) match {
              case "super"    =>
                superComment(sym) foreach { sc =>
                  val superSections = tagIndex(sc)
                  replaceWith(sc.substring(3, startTag(sc, superSections)))
                  for (sec @ (start, end) <- superSections)
                    if (!isMovable(sc, sec)) out append sc.substring(start, end)
                }
              case "" => idx += 1
              case vname  =>
                lookupVariable(vname, site) match {
                  case Some(replacement) => replaceWith(replacement)
                  case None              =>
                    scaladoc.println(s"Variable $vname undefined in comment for $sym in $site")
                }
            }
          }
        if (out.length == 0) str
        else {
          out append str.substring(copied)
          expandInternal(out.toString, depth + 1)
        }
      }

      // We suppressed expanding \$ throughout the recursion, and now we
      // need to replace \$ with $ so it looks as intended.
      expandInternal(initialStr, 0).replace("""\$""", "$")
    }

    def defineVariables(sym: Symbol)(using Context): Unit = {
      val Trim = "(?s)^[\\s&&[^\n\r]]*(.*?)\\s*$".r

      val raw = ctx.docCtx.flatMap(_.docstring(sym).map(_.raw)).getOrElse("")
      defs(sym) ++= defines(raw).map {
        str => {
          val start = skipWhitespace(str, "@define".length)
          val (key, value) = str.splitAt(skipVariable(str, start))
          key.drop(start) -> value
        }
      } map {
        case (key, Trim(value)) =>
          variableName(key) -> value.replaceAll("\\s+\\*+$", "")
      }
    }

    /** Maps symbols to the variable -> replacement maps that are defined
     *  in their doc comments
     */
    private val defs = mutable.HashMap[Symbol, Map[String, String]]() withDefaultValue Map()

    /** Lookup definition of variable.
     *
     *  @param vble  The variable for which a definition is searched
     *  @param site  The class for which doc comments are generated
     */
    def lookupVariable(vble: String, site: Symbol)(using Context): Option[String] = site match {
      case NoSymbol => None
      case _        =>
        val searchList =
          if (site.flags.is(Flags.Module)) site :: site.info.baseClasses
          else site.info.baseClasses

        searchList collectFirst { case x if defs(x) contains vble => defs(x)(vble) } match {
          case Some(str) if str startsWith "$" => lookupVariable(str.tail, site)
          case res                             => res orElse lookupVariable(vble, site.owner)
        }
    }

    /** The position of the raw doc comment of symbol `sym`, or NoPosition if missing
     *  If a symbol does not have a doc comment but some overridden version of it does,
     *  the position of the doc comment of the overridden version is returned instead.
     */
    def docCommentPos(sym: Symbol)(using Context): Span =
      ctx.docCtx.flatMap(_.docstring(sym).map(_.span)).getOrElse(NoSpan)

    /** A version which doesn't consider self types, as a temporary measure:
     *  an infinite loop has broken out between superComment and cookedDocComment
     *  since r23926.
     */
    private def allInheritedOverriddenSymbols(sym: Symbol)(using Context): List[Symbol] =
      if (!sym.owner.isClass) Nil
      else sym.allOverriddenSymbols.toList.filter(_ != NoSymbol) //TODO: could also be `sym.owner.allOverrid..`
      //else sym.owner.ancestors map (sym overriddenSymbol _) filter (_ != NoSymbol)

    class ExpansionLimitExceeded(str: String) extends Exception
  }
}
