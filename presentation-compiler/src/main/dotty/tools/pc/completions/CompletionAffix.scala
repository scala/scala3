package dotty.tools.pc.completions

import org.eclipse.lsp4j.Position
import org.eclipse.lsp4j.Range

/** @param suffixes which we should insert
 *  @param prefixes which we should insert
 *  @param snippet which suffix should we insert the snippet $0
 */
case class CompletionAffix(
    suffixes: Set[Suffix],
    prefixes: Set[Prefix],
    snippet: Suffix,
    currentPrefix: Option[String]
):
  def addLabelSnippet = suffixes.exists(_.kind == SuffixKind.Bracket)
  def hasSnippet = snippet.kind != SuffixKind.NoSuffix
  def chain(copyFn: CompletionAffix => CompletionAffix) = copyFn(this)
  def withNewSuffix(kind: Suffix) = this.copy(suffixes = suffixes + kind)
  def withNewPrefix(kind: Prefix) = this.copy(prefixes = prefixes + kind)
  def withCurrentPrefix(currentPrefix: String) = this.copy(currentPrefix = Some(currentPrefix))
  def withNewSuffixSnippet(suffix: Suffix) =
    this.copy(suffixes = suffixes + suffix, snippet = suffix)

  def nonEmpty: Boolean = suffixes.nonEmpty || prefixes.nonEmpty

  def toSuffix: String =
    def loop(suffixes: List[SuffixKind]): String =
      def cursor = if suffixes.head == snippet.kind then "$0" else ""
      suffixes match
        case SuffixKind.Brace :: tail => s"($cursor)" + loop(tail)
        case SuffixKind.Bracket :: tail => s"[$cursor]" + loop(tail)
        case SuffixKind.Template :: tail => s" {$cursor}" + loop(tail)
        case _ => ""
    loop(suffixes.toList.map(_.kind))

  def toSuffixOpt: Option[String] =
    val edit = toSuffix
    if edit.nonEmpty then Some(edit) else None

  given Ordering[Position] = Ordering.by(elem => (elem.getLine, elem.getCharacter))

  def toInsertRange: Option[Range] =
    import scala.language.unsafeNulls

    val ranges = prefixes.collect:
      case Affix(_, Some(range)) => range
    .toList
    for
      startPos <- ranges.map(_.getStart).minOption
      endPos   <- ranges.map(_.getEnd).maxOption
    yield Range(startPos, endPos)

  private def loopPrefix(prefixes: List[PrefixKind]): String =
    prefixes match
      case PrefixKind.New :: tail => "new " + loopPrefix(tail)
      case PrefixKind.Using :: tail => "using " + loopPrefix(tail)
      case _ => ""

  /** We need to insert previous prefix, but we don't want to display it in the
   *  label i.e.
   *  ```scala
   *  scala.util.Tr @@
   *  ```
   *  should return `new Try[T]: Try[T]` but insert `new scala.util.Try`
   */
  def toInsertPrefix: String =
    loopPrefix(prefixes.toList.map(_.kind)) + currentPrefix.getOrElse("")

  def toPrefix: String =
    loopPrefix(prefixes.toList.map(_.kind))

end CompletionAffix

object CompletionAffix:
  val empty = CompletionAffix(
    suffixes = Set.empty,
    prefixes = Set.empty,
    snippet = Affix(SuffixKind.NoSuffix),
    currentPrefix = None
  )

enum SuffixKind:
  case Brace, Bracket, Template, NoSuffix

enum PrefixKind:
  case New, Using

type Suffix = Affix[SuffixKind]
type Prefix = Affix[PrefixKind]

private case class Affix[+T](kind: T, insertRange: Option[Range] = None)
