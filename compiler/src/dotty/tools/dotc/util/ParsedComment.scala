package dotty.tools.dotc.util

import scala.language.unsafeNulls

import dotty.tools.dotc.core.Comments.{Comment, CommentsContext}
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Names.TermName
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.printing.SyntaxHighlighting

import scala.Console.{BOLD, RESET}
import scala.collection.immutable.ListMap

/**
 * A parsed doc comment.
 *
 * @param comment The doc comment to parse
 */
class ParsedComment(val comment: Comment) {

  /**
   * The bounds of a section that represents the [start; end[ char offset
   * of the section within this comment's `content`.
   */
  private type Bounds = (Int, Int)

  /** The content of this comment, after expansion if possible. */
  val content: String = comment.expandedBody.getOrElse(comment.raw)

  /** An index that marks all sections boundaries */
  private lazy val tagIndex: List[Bounds] = CommentParsing.tagIndex(content)

  /**
   * Maps a parameter name to the bounds of its doc
   *
   * @see paramDoc
   */
  private lazy val paramDocs: Map[String, Bounds] = CommentParsing.paramDocs(content, "@param", tagIndex)

  /**
   * The "main" documentation for this comment. That is, the comment before any section starts.
   */
  lazy val mainDoc: String = {
    val doc = tagIndex match {
      case Nil => content.stripSuffix("*/")
      case (start, _) :: _ => content.slice(0, start)
    }
    clean(doc.stripPrefix("/**"))
  }

  /**
   * Renders this comment as markdown.
   *
   * The different sections are formatted according to the mapping in `knownTags`.
   */
  def renderAsMarkdown(using Context): String =
    val buf = new StringBuilder
    buf.append(mainDoc)
    val groupedSections = CommentParsing.groupedSections(content, tagIndex)

    val sections = for {
      (tag, formatter) <- ParsedComment.knownTags
      boundss <- groupedSections.get(tag)
      texts = boundss.map { case (start, end) => clean(content.slice(start, end)) }
      formatted <- formatter(texts)
    } yield formatted

    if sections.nonEmpty then
      buf.append(System.lineSeparator + System.lineSeparator)
      sections.foreach { section =>
        buf.append(section)
        buf.append(System.lineSeparator)
      }
    buf.toString
  end renderAsMarkdown

  /**
   * The `@param` section corresponding to `name`.
   *
   * @param name The parameter name whose documentation to extract.
   * @return The formatted documentation corresponding to `name`.
   */
  def paramDoc(name: TermName): Option[String] = paramDocs.get(name.toString).map { case (start, end) =>
    val rawContent = content.slice(start, end)
    val docContent = ParsedComment.prefixRegex.replaceFirstIn(rawContent, "")
    clean(docContent)
  }

  /**
   * Cleans `str`: remove prefixing `*` and trim the string.
   *
   * @param str The string to clean
   * @return The cleaned string.
   */
  private def clean(str: String): String = str.stripMargin('*').trim
}

object ParsedComment {

  /**
   * Return the `ParsedComment` associated with `symbol`, if it exists.
   *
   * @param symbol The symbol for which to retrieve the documentation
   * @return If it exists, the `ParsedComment` for `symbol`.
   */
  def docOf(symbol: Symbol)(using Context): Option[ParsedComment] = {
    val documentedSymbol = if (symbol.isPrimaryConstructor) symbol.owner else symbol
    for {
      docCtx <- ctx.docCtx
      comment <- docCtx.docstring(documentedSymbol)
    }
    yield new ParsedComment(comment)
  }

  @scala.annotation.internal.sharable
  private val prefixRegex = """@param\s+\w+\s+""".r

  /** A mapping from tag name to `TagFormatter` */
  private val knownTags: ListMap[String, TagFormatter] = ListMap(
    "@tparam"  -> TagFormatter("Type Parameters", toDescriptionList),
    "@param"   -> TagFormatter("Parameters", toDescriptionList),
    "@return"  -> TagFormatter("Returns", toMarkdownList),
    "@throws"  -> TagFormatter("Throws", toDescriptionList),
    "@see"     -> TagFormatter("See Also", toMarkdownList),
    "@example" -> TagFormatter("Examples", toCodeFences("scala")),
    "@note"    -> TagFormatter("Note", toMarkdownList),
    "@author"  -> TagFormatter("Authors", toMarkdownList),
    "@since"   -> TagFormatter("Since", toMarkdownList),
    "@version" -> TagFormatter("Version", toMarkdownList)
  )

  /**
   * Formats a list of items into a list describing them.
   *
   * Each element is assumed to consist of a first word, which is the item being described. The rest
   * is the description of the item.
   *
   * @param items The items to format into a list.
   * @return A markdown list of descriptions.
   */
  private def toDescriptionList(ctx: Context, items: List[String]): String = inContext(ctx) {
    val formattedItems = items.map { p =>
      val name :: rest = p.split(" ", 2).toList
      s"${bold(name)} ${rest.mkString("").trim}"
    }
    toMarkdownList(ctx, formattedItems)
  }

  /**
   * Formats a list of items into a markdown list.
   *
   * @param items The items to put in a list.
   * @return The list of items, in markdown.
   */
  private def toMarkdownList(ctx: Context, items: List[String]): String = {
    val formattedItems = items.map(_.linesIterator.mkString(System.lineSeparator + "   "))
    formattedItems.mkString(" - ", System.lineSeparator + " - ", "")
  }

  /**
   * If the color is enabled, add syntax highlighting to each of `snippets`, otherwise wrap each
   * of them in a markdown code fence.
   * The results are put into a markdown list.
   *
   * @param language The language to use for the code fences
   * @param snippets The list of snippets to format.
   * @return A markdown list of code fences.
   * @see toCodeFence
   */
  private def toCodeFences(language: String)(ctx: Context, snippets: List[String]): String =
    toMarkdownList(ctx, snippets.map(toCodeFence(language)(ctx, _)))

  /**
   * Formats `snippet` for display. If the color is enabled, the syntax is highlighted,
   * otherwise the snippet is wrapped in a markdown code fence.
   *
   * @param language The language to use.
   * @param snippet  The code snippet
   * @return `snippet`, wrapped in a code fence.
   */
  private def toCodeFence(language: String)(ctx: Context, snippet: String): String = inContext(ctx) {
    if colorEnabled then
      SyntaxHighlighting.highlight(snippet)
    else
      s"""```$language
         |$snippet
         |```""".stripMargin
  }

  /**
   * Format the elements of documentation associated with a given tag using `fn`, and starts the
   * section with `title`.
   *
   * @param title The title to give to the formatted items.
   * @param fn    The formatting function to use.
   */
  private case class TagFormatter(title: String, fn: (Context, List[String]) => String) {

    /**
     * Format `item` using `fn` if `items` is not empty.
     *
     * @param items The items to format
     * @return If items is not empty, the items formatted using `fn`.
     */
    def apply(items: List[String])(using Context): Option[String] = items match {
      case Nil =>
        None
      case items =>
        Some(s"""${bold(title)}
                |${fn(ctx, items)}
                |""".stripMargin)
    }
  }

  /** Is the color enabled in the context? */
  private def colorEnabled(using Context): Boolean =
    ctx.settings.color.value != "never"

  /** Show `str` in bold */
  private def bold(str: String)(using Context): String =
    if (colorEnabled) s"$BOLD$str$RESET"
    else s"**$str**"
}

