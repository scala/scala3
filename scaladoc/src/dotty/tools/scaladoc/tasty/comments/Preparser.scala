package dotty.tools.scaladoc
package tasty.comments

import scala.collection.mutable
import scala.collection.immutable.SortedMap
import scala.util.matching.Regex

object Preparser {
  import Regexes._

  /** Parses a raw comment string into a `Comment` object. */
  def preparse(
    comment: List[String],
  ): PreparsedComment = {

    /** Parses a comment (in the form of a list of lines) to a `Comment`
      * instance, recursively on lines. To do so, it splits the whole comment
      * into main body and tag bodies, then runs the `WikiParser` on each body
      * before creating the comment instance.
      *
      * @param docBody     The body of the comment parsed until now.
      * @param tags        All tags parsed until now.
      * @param lastTagKey  The last parsed tag, or `None` if the tag section
      *                    hasn't started. Lines that are not tagged are part
      *                    of the previous tag or, if none exists, of the body.
      * @param remaining   The lines that must still recursively be parsed.
      * @param inCodeBlock Whether the next line is part of a code block (in
      *                    which no tags must be read).
      */
    def go(
      docBody: StringBuilder,
      tags: Map[TagKey, List[String]],
      lastTagKey: Option[TagKey],
      remaining: List[String],
      inCodeBlock: Boolean,
    )(using strippedLinesBeforeNo: Int = 0): PreparsedComment = remaining match {
      case CodeBlockStartRegex(before, marker, after) :: ls if !inCodeBlock =>
        if (!before.trim.isEmpty && !after.trim.isEmpty && marker == "```")
          go(docBody, tags, lastTagKey, before :: (marker + after) :: ls, inCodeBlock = false)
        else if (!before.trim.isEmpty && !after.trim.isEmpty)
          go(docBody, tags, lastTagKey, before :: marker :: after :: ls, inCodeBlock = false)
        else if (!before.trim.isEmpty)
          go(docBody, tags, lastTagKey, before :: marker :: ls, inCodeBlock = false)
        else if (!after.trim.isEmpty && marker != "```")
          go(docBody, tags, lastTagKey, marker :: after :: ls, inCodeBlock = true)
        else lastTagKey match {
          case Some(key) =>
            val value =
              ((tags get key): @unchecked) match {
                case Some(b :: bs) => (b + endOfLine + marker) :: bs
                case None => oops("lastTagKey set when no tag exists for key")
              }
            go(docBody, tags + (key -> value), lastTagKey, ls, inCodeBlock = true)
          case None =>
            go(docBody append endOfLine append (marker + after), tags, lastTagKey, ls, inCodeBlock = true)
        }

      case CodeBlockEndRegex(before, marker, after) :: ls =>
        if (!before.trim.isEmpty && !after.trim.isEmpty)
          go(docBody, tags, lastTagKey, before :: marker :: after :: ls, inCodeBlock = true)
        if (!before.trim.isEmpty)
          go(docBody, tags, lastTagKey, before :: marker :: ls, inCodeBlock = true)
        else if (!after.trim.isEmpty)
          go(docBody, tags, lastTagKey, marker :: after :: ls, inCodeBlock = false)
        else lastTagKey match {
          case Some(key) =>
            val value =
              ((tags get key): @unchecked) match {
                case Some(b :: bs) => (b + endOfLine + marker) :: bs
                case None => oops("lastTagKey set when no tag exists for key")
              }
            go(docBody, tags + (key -> value), lastTagKey, ls, inCodeBlock = false)
          case None =>
            go(docBody append endOfLine append marker, tags, lastTagKey, ls, inCodeBlock = false)
        }


      case SymbolTagRegex(name, sym, body) :: ls if !inCodeBlock =>
        val key = SymbolTagKey(name, sym)
        val value = body :: tags.getOrElse(key, Nil)
        go(docBody, tags + (key -> value), Some(key), ls, inCodeBlock)

      case SimpleTagRegex(name, body) :: ls if !inCodeBlock =>
        val key = SimpleTagKey(name)
        val value = body :: tags.getOrElse(key, Nil)
        go(docBody, tags + (key -> value), Some(key), ls, inCodeBlock)


      case SingleTagRegex(name) :: ls if !inCodeBlock =>
        val key = SimpleTagKey(name)
        val value = "" :: tags.getOrElse(key, Nil)
        go(docBody, tags + (key -> value), Some(key), ls, inCodeBlock)


      case line :: ls if lastTagKey.isDefined =>
        val newtags = if !line.isEmpty then {
          val key = lastTagKey.get
          val value =
            ((tags get key): @unchecked) match {
              case Some(b :: bs) => (b + endOfLine + line) :: bs
              case None => oops("lastTagKey set when no tag exists for key")
            }
          tags + (key -> value)
        } else tags
        go(docBody, newtags, lastTagKey, ls, inCodeBlock)


      case line :: ls =>
        if docBody.length > 0 then docBody.append(endOfLine)
        docBody.append(line)
        go(docBody, tags, lastTagKey, ls, inCodeBlock)(using strippedLinesBeforeNo + (if line.isEmpty && docBody.length == 0 then 1 else 0))


      case Nil =>
        // Take the {inheritance, content} diagram keys aside, as it doesn't need any parsing
        val inheritDiagramTag = SimpleTagKey("inheritanceDiagram")
        val contentDiagramTag = SimpleTagKey("contentDiagram")

        val inheritDiagramText: List[String] = tags.get(inheritDiagramTag) match {
          case Some(list) => list
          case None => List.empty
        }

        val contentDiagramText: List[String] = tags.get(contentDiagramTag) match {
          case Some(list) => list
          case None => List.empty
        }

        val stripTags = List(inheritDiagramTag, contentDiagramTag, SimpleTagKey("template"), SimpleTagKey("documentable"))
        val tagsWithoutDiagram = tags.filterNot(pair => stripTags.contains(pair._1))

        val bodyTags: mutable.Map[TagKey, List[String]] =
          mutable.Map((tagsWithoutDiagram).toSeq: _*)

        def allTags(key: SimpleTagKey): List[String] =
          (bodyTags remove key).getOrElse(Nil).reverse

        def allSymsOneTag(key: TagKey, filterEmpty: Boolean = true): SortedMap[String, String] = {
          val keys: Seq[SymbolTagKey] =
            bodyTags.keys.toSeq flatMap {
              case stk: SymbolTagKey if (stk.name == key.name) => Some(stk)
              case stk: SimpleTagKey if (stk.name == key.name) =>
                // scaladoc.println(s"$span: tag '@${stk.name}' must be followed by a symbol name")
                None
              case _ => None
            }
          val pairs: Seq[(String, String)] =
            for (key <- keys) yield {
              val bs = (bodyTags remove key).get
              // if (bs.length > 1)
                // scaladoc.println(s"$span: only one '@${key.name}' tag for symbol ${key.symbol} is allowed")
              (key.symbol, bs.head)
            }
          SortedMap.empty[String, String] ++ pairs
        }

        val cmt = PreparsedComment(
          body                    = docBody.toString,
          authors                 = allTags(SimpleTagKey("author")),
          see                     = allTags(SimpleTagKey("see")),
          result                  = allTags(SimpleTagKey("return")),
          throws                  = allSymsOneTag(SimpleTagKey("throws")),
          valueParams             = allSymsOneTag(SimpleTagKey("param")),
          typeParams              = allSymsOneTag(SimpleTagKey("tparam")),
          version                 = allTags(SimpleTagKey("version")),
          since                   = allTags(SimpleTagKey("since")),
          todo                    = allTags(SimpleTagKey("todo")),
          deprecated              = allTags(SimpleTagKey("deprecated")),
          note                    = allTags(SimpleTagKey("note")),
          example                 = allTags(SimpleTagKey("example")),
          constructor             = allTags(SimpleTagKey("constructor")),
          group                   = allTags(SimpleTagKey("group")),
          groupDesc               = allSymsOneTag(SimpleTagKey("groupdesc")),
          groupNames              = allSymsOneTag(SimpleTagKey("groupname")),
          groupPrio               =
            allSymsOneTag(SimpleTagKey("groupprio")).transform((_, v) => v.toInt),
          hideImplicitConversions = allTags(SimpleTagKey("hideImplicitConversion")),
          shortDescription        = allTags(SimpleTagKey("shortDescription")),
          syntax                  = allTags(SimpleTagKey("syntax")),
          strippedLinesBeforeNo   = strippedLinesBeforeNo
        )

        cmt
    }

    go(new StringBuilder(comment.size), Map.empty, None, comment, inCodeBlock = false)
  }

  /** A key used for a tag map. The key is built from the name of the tag and
    * from the linked symbol if the tag has one.
    * Equality on tag keys is structural. */
  private sealed abstract class TagKey {
    def name: String
  }

  private case class SimpleTagKey(name: String) extends TagKey
  private case class SymbolTagKey(name: String, symbol: String) extends TagKey

  /** Something that should not have happened, happened, and Scaladoc should exit. */
  private def oops(msg: String): Nothing =
    throw new IllegalArgumentException("program logic: " + msg)
}
