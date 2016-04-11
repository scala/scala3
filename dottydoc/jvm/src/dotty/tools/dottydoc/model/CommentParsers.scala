package dotty.tools
package dottydoc
package model

object CommentParsers {
  import comment._
  import BodyParsers._

  sealed class WikiParser
  extends CommentCleaner with CommentParser with CommentCooker {
    def parseHtml(str: String): String =
      parse(clean(str), str).toHtml
  }

  val wikiParser = new WikiParser
}
