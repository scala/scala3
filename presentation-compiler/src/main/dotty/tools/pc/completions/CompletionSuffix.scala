package dotty.tools.pc.completions

/**
 * @param suffixes which we should insert
 * @param snippet which suffix should we insert the snippet $0
 */
case class CompletionSuffix(
    suffixes: Set[SuffixKind],
    snippet: SuffixKind,
):
  def labelSnippet =
    suffixes.collectFirst {
      case SuffixKind.Bracket(1) => "[T]"
      case SuffixKind.Bracket(n) =>
        (for (i <- 1 to n) yield s"T$i").mkString("[", ", ", "]")
    }
  def hasSnippet = snippet != SuffixKind.NoSuffix
  def chain(copyFn: CompletionSuffix => CompletionSuffix) = copyFn(this)
  def withNewSuffix(kind: SuffixKind) =
    CompletionSuffix(suffixes + kind, snippet)
  def withNewSuffixSnippet(kind: SuffixKind) =
    CompletionSuffix(suffixes + kind, kind)
  def toEdit: String =
    def loop(suffixes: List[SuffixKind]): String =
      def cursor = if suffixes.head == snippet then "$0" else ""
      suffixes match
        case SuffixKind.Brace :: tail => s"($cursor)" + loop(tail)
        case SuffixKind.Bracket(_) :: tail => s"[$cursor]" + loop(tail)
        case SuffixKind.Template :: tail => s" {$cursor}" + loop(tail)
        case _ => ""
    loop(suffixes.toList)
  def toEditOpt: Option[String] =
    val edit = toEdit
    if edit.nonEmpty then Some(edit) else None
end CompletionSuffix

object CompletionSuffix:
  val empty = CompletionSuffix(
    suffixes = Set.empty,
    snippet = SuffixKind.NoSuffix,
  )

enum SuffixKind:
  case Brace extends SuffixKind
  case Bracket(typeParamsCount: Int) extends SuffixKind
  case Template extends SuffixKind
  case NoSuffix extends SuffixKind
