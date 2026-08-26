package dotty.tools.scaladoc
package snippets

case class SnippetSource(
  snippet: String,
  // For each physical line in `snippet`, record the original source line if it came
  // from user code. Synthetic wrapper lines around imported preambles use `None`.
  sourceLines: IndexedSeq[Option[Int]],
  outerLineOffset: Int
):
  def withPreamble(preamble: SnippetSource): SnippetSource =
    if preamble.snippet.isEmpty then this
    else if snippet.isEmpty then preamble
    else SnippetSource(
      s"${preamble.snippet}\n$snippet",
      preamble.sourceLines ++ sourceLines,
      outerLineOffset
    )

  def append(other: SnippetSource): SnippetSource =
    if snippet.isEmpty then other
    else if other.snippet.isEmpty then this
    else SnippetSource(
      s"$snippet\n${other.snippet}",
      sourceLines ++ other.sourceLines,
      outerLineOffset
    )

  def wrapAsImport(id: String): SnippetSource =
    val start = s"//{i:$id"
    val end = "//i}"
    SnippetSource(
      Seq(start, snippet, end).filter(_.nonEmpty).mkString("\n"),
      IndexedSeq(None) ++ sourceLines ++ IndexedSeq(None),
      outerLineOffset
    )

object SnippetSource:
  val empty = SnippetSource("", IndexedSeq.empty, 0)

  def apply(snippet: String, outerLineOffset: Int): SnippetSource =
    val normalizedSnippet = snippet.stripSuffix("\n")
    val lines = WrappedSnippet.splitLines(normalizedSnippet)
    val sourceLines = lines.indices.map(idx => Some(outerLineOffset + idx))
    SnippetSource(normalizedSnippet, sourceLines, outerLineOffset)
