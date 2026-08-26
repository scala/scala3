package dotty.tools.pc

import dotty.tools.dotc.core.Comments.Comment

object ScriptFirstImportPosition:

  val usingDirectives: List[String] = List("// using", "//> using")
  val ammHeaders: List[String] = List("// scala", "// ammonite")

  def ammoniteScStartOffset(
      comments: List[Comment]
  ): Option[Int] =
    findStartOffset(comments, commentQuery = "/*<start>*/", ammHeaders)

  def scalaCliScStartOffset(
      comments: List[Comment]
  ): Option[Int] =
    findStartOffset(
      comments,
      commentQuery = "/*<script>*/",
      usingDirectives
    )

  def findStartOffset(
      comments: List[Comment],
      commentQuery: String,
      excludedComments: List[String]
  ): Option[Int] =
    val startComment =
      Option(comments.indexWhere(_.raw == commentQuery)).filter(_ >= 0)
    startComment.flatMap { startIndex =>
      val commentsInsideScript = comments.drop(startIndex + 1)
      val (headers, _) =
        commentsInsideScript.span(comment =>
          excludedComments.exists(comment.raw.startsWith)
        )
      if headers.isEmpty then Some(comments(startIndex).span.end + 1)
      else headers.lastOption.map(_.span.end + 1)
    }
