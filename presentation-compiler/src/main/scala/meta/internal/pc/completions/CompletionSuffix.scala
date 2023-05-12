package scala.meta.internal.pc.completions

/**
 * @param brace should we add "()" suffix?
 * @param bracket should we add "[]" suffix?
 * @param template should we add "{}" suffix?
 * @param snippet which suffix should we insert the snippet $0
 */
case class CompletionSuffix(
    brace: Boolean,
    bracket: Boolean,
    template: Boolean,
    snippet: SuffixKind,
):
  def hasSnippet = snippet != SuffixKind.NoSuffix
  def chain(copyFn: CompletionSuffix => CompletionSuffix) = copyFn(this)
  def toEdit: String =
    if !hasSuffix then ""
    else
      val braceSuffix =
        if brace && snippet == SuffixKind.Brace then "($0)"
        else if brace then "()"
        else ""
      val bracketSuffix =
        if bracket && snippet == SuffixKind.Bracket then "[$0]"
        else if bracket then "[]"
        else ""
      val templateSuffix =
        if template && snippet == SuffixKind.Template then " {$0}"
        else if template then " {}"
        else ""
      s"$bracketSuffix$braceSuffix$templateSuffix"
  def toEditOpt: Option[String] =
    val edit = toEdit
    if edit.nonEmpty then Some(edit) else None
  private def hasSuffix = brace || bracket || template
end CompletionSuffix

object CompletionSuffix:
  val empty = CompletionSuffix(
    brace = false,
    bracket = false,
    template = false,
    snippet = SuffixKind.NoSuffix,
  )

enum SuffixKind:
  case Brace, Bracket, Template, NoSuffix
