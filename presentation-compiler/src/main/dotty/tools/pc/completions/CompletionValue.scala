package dotty.tools.pc
package completions

import scala.meta.internal.pc.CompletionItemData

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Denotations.Denotation
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Types.Type
import dotty.tools.pc.printer.ShortenedTypePrinter
import dotty.tools.pc.utils.InteractiveEnrichments.decoded

import org.eclipse.lsp4j.CompletionItemKind
import org.eclipse.lsp4j.CompletionItemTag
import org.eclipse.lsp4j.InsertTextMode
import org.eclipse.lsp4j.Range
import org.eclipse.lsp4j.TextEdit

enum CompletionSource:
  case Empty
  case OverrideKind
  case ImplementAllKind
  case CompilerKind
  case KeywordKind
  case ScopeKind
  case WorkspaceKind
  case ExtensionKind
  case NamedArgKind
  case AutoFillKind
  case FileSystemMemberKind
  case IvyImportKind
  case InterpolatorKind
  case MatchCompletionKind
  case CaseKeywordKind
  case DocumentKind
  case ImplicitClassKind

sealed trait CompletionValue:
  def label: String
  def insertText: Option[String] = None
  def snippetAffix: CompletionAffix = CompletionAffix.empty
  def additionalEdits: List[TextEdit] = Nil
  def range: Option[Range] = None
  def filterText: Option[String] = None
  def completionItemKind(using Context): CompletionItemKind
  def completionItemDataKind: Integer = CompletionItemData.None
  def description(printer: ShortenedTypePrinter)(using Context): String = ""
  def insertMode: Option[InsertTextMode] = None
  def completionData(buildTargetIdentifier: String)(
    using Context
  ): CompletionItemData = CompletionItemData("<no-symbol>", buildTargetIdentifier, kind = completionItemDataKind)
  def command: Option[String] = None

  /**
   * Label with potentially attached description.
   */
  def labelWithDescription(printer: ShortenedTypePrinter)(using Context): String =
    label
  def lspTags(using Context): List[CompletionItemTag] = Nil
end CompletionValue

object CompletionValue:

  sealed trait Symbolic extends CompletionValue:
    def denotation: Denotation
    val symbol = denotation.symbol
    override def completionItemDataKind = CompletionItemData.None
    def isExtensionMethod: Boolean = false

    override def completionData(
        buildTargetIdentifier: String
    )(using Context): CompletionItemData =
      CompletionItemData(
        SemanticdbSymbols.symbolName(symbol),
        buildTargetIdentifier,
        kind = completionItemDataKind
      )
    def importSymbol: Symbol = symbol

    override def range: Option[Range] =
      snippetAffix.toInsertRange

    def completionItemKind(using Context): CompletionItemKind =
      val symbol = this.symbol
      if symbol.is(Package) || symbol.is(Module) then
        // No CompletionItemKind.Package (https://github.com/Microsoft/language-server-protocol/issues/155)
        CompletionItemKind.Module
      else if symbol.isConstructor then CompletionItemKind.Constructor
      else if symbol.isClass then CompletionItemKind.Class
      else if symbol.is(Mutable) then CompletionItemKind.Variable
      else if symbol.is(Method) then CompletionItemKind.Method
      else CompletionItemKind.Field

    override def lspTags(using Context): List[CompletionItemTag] =
      if symbol.isDeprecated then List(CompletionItemTag.Deprecated) else Nil

    override def labelWithDescription(
        printer: ShortenedTypePrinter
    )(using Context): String =
      if symbol.isConstructor then s"${snippetAffix.toPrefix}${label}${description(printer)}"
      else if symbol.is(Method) then s"${label}${description(printer)}"
      else if symbol.is(Mutable) then s"$label${description(printer)}"
      else if symbol.is(Package) || symbol.is(Module) || symbol.isClass then
        s"${labelWithSuffix(printer)}${description(printer)}"
      else if symbol.isType then labelWithSuffix(printer)
      else if symbol.isTerm && symbol.info.typeSymbol.is(Module) then
        s"${label}${description(printer)}"
      else s"$label${description(printer)}"

    protected def labelWithSuffix(printer: ShortenedTypePrinter)(using Context): String =
      if snippetAffix.addLabelSnippet
      then
        val printedParams = symbol.info.typeParams.map(p =>
          p.paramName.decoded ++ printer.tpe(p.paramInfo)
        )
        s"${label}${printedParams.mkString("[", ",", "]")}"
      else label

    override def description(printer: ShortenedTypePrinter)(using Context): String =
      def info = denotation.info.widenTermRefExpr
      val isVal = !(symbol.is(Module) || symbol.is(Method) || symbol.isType || info.typeSymbol.is(Module))
      val prefix = if isVal then ": " else ""
      prefix ++ printer.completionSymbol(denotation)

  end Symbolic

  case class Compiler(
      label: String,
      denotation: Denotation,
      override val snippetAffix: CompletionAffix
  ) extends Symbolic:
    override def completionItemDataKind: Integer = CompletionSource.CompilerKind.ordinal

  /**
    *  We need to access original completion in sorting phase.
    *  This class is only a wrapper to hold both new completion and original completion.
    *
    *  All methods are proxied to @param extraMethod
    *
    *  FIXME Refactor this file to different architercture. At least to somethhing that is easier to modifiy and scale.
    *  One solution may be a migration to flag based solution.
    */
  case class ExtraMethod(
      owner: Denotation,
      extraMethod: Symbolic,
  ) extends Symbolic:
    override def additionalEdits: List[TextEdit] = extraMethod.additionalEdits
    override def command: Option[String] = extraMethod.command
    override def completionData(buildTargetIdentifier: String)(using Context): CompletionItemData = extraMethod.completionData((buildTargetIdentifier))
    override def completionItemKind(using Context): CompletionItemKind = extraMethod.completionItemKind
    override def description(printer: ShortenedTypePrinter)(using Context): String = extraMethod.description(printer)
    override def labelWithDescription(printer: ShortenedTypePrinter)(using Context): String = extraMethod.labelWithDescription(printer)
    override def range: Option[Range] = extraMethod.range
    override def denotation: Denotation = extraMethod.denotation
    override def label: String = extraMethod.label
    override def filterText: Option[String] = extraMethod.filterText
    override def importSymbol: Symbol = extraMethod.importSymbol
    override def lspTags(using Context): List[CompletionItemTag] = extraMethod.lspTags
    override def insertText: Option[String] = extraMethod.insertText
    override def isExtensionMethod: Boolean = extraMethod.isExtensionMethod
    override def snippetAffix: CompletionAffix = extraMethod.snippetAffix
    override def insertMode: Option[InsertTextMode] = extraMethod.insertMode
    override val symbol: Symbol = extraMethod.symbol
    override def completionItemDataKind: Integer = extraMethod.completionItemDataKind

  case class Scope(
      label: String,
      denotation: Denotation,
      override val snippetAffix: CompletionAffix,
  ) extends Symbolic:
    override def completionItemDataKind: Integer = CompletionSource.ScopeKind.ordinal

  case class Workspace(
      label: String,
      denotation: Denotation,
      override val snippetAffix: CompletionAffix,
      override val importSymbol: Symbol
  ) extends Symbolic:
    override def completionItemDataKind: Integer = CompletionSource.WorkspaceKind.ordinal

    override def labelWithDescription(printer: ShortenedTypePrinter)(using Context): String =
      def isMethodOrValue = !(symbol.isType || symbol.is(Module))
      if symbol.isConstructor || symbol.name == nme.apply then
        s"${snippetAffix.toPrefix}${label}${description(printer)} - ${printer.fullNameString(importSymbol.effectiveOwner)}"
      else if isMethodOrValue then
        s"${labelWithSuffix(printer)} - ${printer.fullNameString(symbol.effectiveOwner)}"
      else if symbol.is(Package) || symbol.is(Module) || symbol.isClass then
        s"${labelWithSuffix(printer)} -${description(printer)}"
      else super.labelWithDescription(printer)

  /**
   * CompletionValue for old implicit classes methods via SymbolSearch
   */
  case class ImplicitClass(
      label: String,
      denotation: Denotation,
      override val snippetAffix: CompletionAffix,
      override val importSymbol: Symbol,
  ) extends Symbolic:
    override def completionItemKind(using Context): CompletionItemKind =
      CompletionItemKind.Method
    override def completionItemDataKind: Integer = CompletionSource.ImplicitClassKind.ordinal
    override def description(printer: ShortenedTypePrinter)(using Context): String =
      s"${super.description(printer)} (implicit)"

  /**
   * CompletionValue for extension methods via SymbolSearch
   */
  case class Extension(
      label: String,
      denotation: Denotation,
      override val snippetAffix: CompletionAffix
  ) extends Symbolic:
    override def completionItemKind(using Context): CompletionItemKind =
      CompletionItemKind.Method
    override def completionItemDataKind: Integer = CompletionSource.ExtensionKind.ordinal
    override def isExtensionMethod: Boolean = true
    override def description(printer: ShortenedTypePrinter)(using Context): String =
      s"${printer.completionSymbol(denotation)} (extension)"

  /**
   * @param shortenedNames shortened type names by `Printer`. This field should be used for autoImports
   * @param start Starting position of the completion
   *              this is needed, because for OverrideCompletion, completionPos
   *              doesn't capture the "correct" starting position. For example,
   *              when we type `override def fo@@` (where `@@` we invoke completion)
   *              `completionPos` is `fo`, instead of `override def fo`.
   */
  case class Override(
      label: String,
      value: String,
      denotation: Denotation,
      override val additionalEdits: List[TextEdit],
      override val filterText: Option[String],
      override val range: Option[Range]
  ) extends Symbolic:
    override def insertText: Option[String] = Some(value)
    override def completionItemDataKind: Integer = CompletionSource.OverrideKind.ordinal
    override def completionItemKind(using Context): CompletionItemKind =
      CompletionItemKind.Method
    override def labelWithDescription(printer: ShortenedTypePrinter)(using Context): String =
      label
  end Override

  case class NamedArg(
      label: String,
      tpe: Type,
      denotation: Denotation
  ) extends Symbolic:
    override def insertText: Option[String] = Some(label.replace("$", "$$").nn)
    override def completionItemDataKind: Integer = CompletionSource.OverrideKind.ordinal
    override def completionItemKind(using Context): CompletionItemKind =
      CompletionItemKind.Field
    override def description(printer: ShortenedTypePrinter)(using Context): String =
      ": " + printer.tpe(tpe)

    override def labelWithDescription(printer: ShortenedTypePrinter)(using Context): String =
      label
  end NamedArg

  case class Autofill(
      value: String,
      override val label: String,
  ) extends CompletionValue:
    override def completionItemKind(using Context): CompletionItemKind =
      CompletionItemKind.Enum
    override def completionItemDataKind: Integer = CompletionSource.OverrideKind.ordinal
    override def insertText: Option[String] = Some(value)

  case class Keyword(label: String, override val insertText: Option[String])
      extends CompletionValue:
    override def completionItemDataKind: Integer = CompletionSource.KeywordKind.ordinal
    override def completionItemKind(using Context): CompletionItemKind =
      CompletionItemKind.Keyword

  case class FileSystemMember(
      filename: String,
      override val range: Option[Range],
      isDirectory: Boolean
  ) extends CompletionValue:
    override def label: String = filename
    override def insertText: Option[String] = Some(filename.stripSuffix(".sc"))
    override def completionItemDataKind: Integer = CompletionSource.FileSystemMemberKind.ordinal
    override def completionItemKind(using Context): CompletionItemKind =
      CompletionItemKind.File

  case class IvyImport(
      label: String,
      override val insertText: Option[String],
      override val range: Option[Range]
  ) extends CompletionValue:
    override val filterText: Option[String] = insertText
    override def completionItemDataKind: Integer = CompletionSource.IvyImportKind.ordinal
    override def completionItemKind(using Context): CompletionItemKind =
      CompletionItemKind.Folder

  // TODO remove this type and return `Compiler`, `Workspace` instead
  case class Interpolator(
      denotation: Denotation,
      label: String,
      override val insertText: Option[String],
      override val additionalEdits: List[TextEdit],
      override val range: Option[Range],
      override val filterText: Option[String],
      override val importSymbol: Symbol,
      isWorkspace: Boolean = false,
      isExtension: Boolean = false
  ) extends Symbolic:
    override def completionItemDataKind: Integer = CompletionSource.InterpolatorKind.ordinal
    override def description(
        printer: ShortenedTypePrinter
    )(using Context): String =
      if isExtension then s"${printer.completionSymbol(symbol)} (extension)"
      else super.description(printer)
    override def isExtensionMethod: Boolean = isExtension
  end Interpolator

  case class MatchCompletion(
      label: String,
      override val insertText: Option[String],
      override val additionalEdits: List[TextEdit],
      desc: String
  ) extends CompletionValue:
    override def completionItemDataKind: Integer = CompletionSource.MatchCompletionKind.ordinal
    override def completionItemKind(using Context): CompletionItemKind =
      CompletionItemKind.Enum
    override def description(printer: ShortenedTypePrinter)(using Context): String =
      desc

  case class CaseKeyword(
      denotation: Denotation,
      label: String,
      override val insertText: Option[String],
      override val additionalEdits: List[TextEdit],
      override val range: Option[Range] = None,
      override val command: Option[String] = None
  ) extends Symbolic:
    override def completionItemDataKind: Integer = CompletionSource.CaseKeywordKind.ordinal
    override def completionItemKind(using Context): CompletionItemKind =
      CompletionItemKind.Method

    override def labelWithDescription(printer: ShortenedTypePrinter)(using Context): String =
      label

    override def description(printer: ShortenedTypePrinter)(using Context): String =
       printer.completionSymbol(denotation)
  end CaseKeyword

  case class Document(label: String, doc: String, description: String)
      extends CompletionValue:
    override def filterText: Option[String] = Some(description)

    override def insertText: Option[String] = Some(doc)
    override def completionItemDataKind: Integer = CompletionSource.DocumentKind.ordinal
    override def completionItemKind(using Context): CompletionItemKind =
      CompletionItemKind.Snippet

    override def description(printer: ShortenedTypePrinter)(using Context): String =
      description
    override def insertMode: Option[InsertTextMode] = Some(InsertTextMode.AsIs)

  case class SingletonValue(label: String, info: Type, override val range: Option[Range])
    extends CompletionValue:
      override def insertText: Option[String] = Some(label)
      override def labelWithDescription(printer: ShortenedTypePrinter)(using Context): String =
        s"$label: ${printer.tpe(info)}"

      override def completionItemKind(using Context): CompletionItemKind =
        CompletionItemKind.Constant

  def namedArg(label: String, sym: ParamSymbol)(using
      Context
  ): CompletionValue =
    NamedArg(label, sym.info.widenTermRefExpr, sym.symbol)

  def keyword(label: String, insertText: String): CompletionValue =
    Keyword(label, Some(insertText))

  def document(
      label: String,
      insertText: String,
      description: String
  ): CompletionValue =
    Document(label, insertText, description)

end CompletionValue
