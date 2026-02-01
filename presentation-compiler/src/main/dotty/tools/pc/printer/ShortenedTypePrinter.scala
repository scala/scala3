package dotty.tools.pc.printer

import scala.collection.mutable
import scala.meta.internal.jdk.CollectionConverters.*
import scala.meta.internal.mtags.KeywordWrapper
import scala.meta.pc.SymbolDocumentation
import scala.meta.pc.SymbolSearch
import scala.meta.pc.reports.ReportContext

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Denotations.Denotation
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.NameKinds.ContextBoundParamName
import dotty.tools.dotc.core.NameOps.*
import dotty.tools.dotc.core.Names
import dotty.tools.dotc.core.Names.Name
import dotty.tools.dotc.core.Names.NameOrdering
import dotty.tools.dotc.core.StdNames
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.core.Types.Type
import dotty.tools.dotc.printing.RefinedPrinter
import dotty.tools.dotc.printing.Texts.Text
import dotty.tools.pc.AutoImports.AutoImportsGenerator
import dotty.tools.pc.AutoImports.ImportSel
import dotty.tools.pc.IndexedContext
import dotty.tools.pc.IndexedContext.Result
import dotty.tools.pc.Params
import dotty.tools.pc.utils.InteractiveEnrichments.*

import org.eclipse.lsp4j.TextEdit

/** A type printer that shortens types by replacing fully qualified names with
 *  shortened versions.
 *
 *  The printer supports symbol renames found in scope and will use the rename
 *  if it is available. It also handle custom renames as specified in the
 *  `renameConfigMap` parameter.
 */
class ShortenedTypePrinter(
    symbolSearch: SymbolSearch,
    includeDefaultParam: ShortenedTypePrinter.IncludeDefaultParam =
      IncludeDefaultParam.ResolveLater,
    isTextEdit: Boolean = false,
    renameConfigMap: Map[Symbol, String] = Map.empty
)(using indexedCtx: IndexedContext, reportCtx: ReportContext) extends RefinedPrinter(indexedCtx.ctx):
  private val missingImports: mutable.Set[ImportSel] = mutable.LinkedHashSet.empty
  private val defaultWidth = 1000

  private val methodFlags =
    Flags.commonFlags(
      Private,
      Protected,
      Final,
      Implicit,
      Given,
      Override,
      Transparent,
      Erased,
      Inline,
      AbsOverride,
      Lazy
    )

  private val foundRenames = collection.mutable.LinkedHashMap.empty[Symbol, String]

  override def nameString(name: Name): String =
    val nameStr = super.nameString(name)
    if nameStr.nonEmpty then KeywordWrapper.Scala3Keywords.backtickWrap(nameStr)
    else nameStr

  def getUsedRenames: Map[Symbol, String] =
    foundRenames.toMap.filter { case (k, v) => k.showName != v }

  def getUsedRenamesInfo(using Context): List[String] =
    foundRenames.map { (from, to) =>
      s"type $to = ${from.showName}"
    }.toList

  def expressionType(tpw: Type)(using Context): Option[String] =
    tpw match
      case t: PolyType =>
        expressionType(t.resType)
      case t: MethodType =>
        expressionType(t.resType)
      case i: ImportType =>
        expressionType(i.expr.typeOpt)
      case c: ConstantType =>
        Some(tpe(c.underlying))
      case _ if !tpw.isErroneous =>
        Some(tpe(tpw))
      case _ => None

  /** Returns a list of TextEdits (auto-imports) of the symbols
   */
  def imports(autoImportsGen: AutoImportsGenerator): List[TextEdit] =
    missingImports
      .toList
      .filterNot(selector => selector.sym.isRoot)
      .sortBy(_.sym.effectiveName)
      .flatMap(selector => autoImportsGen.renderImports(List(selector)))

  sealed trait SymbolRenameSearchResult:
    val owner: Symbol
    val rename: String
    val prefixAfterRename: List[Symbol]

    def toPrefixText: Text =
      Str(rename) ~ prefixAfterRename.foldLeft(Text())((acc, sym) =>
        acc ~ "." ~ toText(sym.name)
      ) ~ "."

  case class Found(owner: Symbol, rename: String, prefixAfterRename: List[Symbol]) extends SymbolRenameSearchResult
  case class Missing(owner: Symbol, rename: String, prefixAfterRename: List[Symbol]) extends SymbolRenameSearchResult

  /** In shortened type printer, we don't want to omit the prefix unless it is
   *  empty package All the logic for prefix omitting is implemented in
   *  `toTextPrefixOf`
   */
  override protected def isOmittablePrefix(sym: Symbol): Boolean =
    isEmptyPrefix(sym)

  private def findPrefixRename(
      prefix: Symbol
  ): Option[SymbolRenameSearchResult] =
    def ownersAfterRename(owner: Symbol): List[Symbol] =
      prefix.ownersIterator.takeWhile(_ != owner).toList

    val prefixIterator = if isTextEdit then prefix.ownersIterator else Iterator(prefix)

    prefixIterator.flatMap { owner =>
      val prefixAfterRename = ownersAfterRename(owner)
      val ownerRename = indexedCtx.rename(owner)
      ownerRename.foreach(rename => foundRenames += owner -> rename)
      val currentRenamesSearchResult =
        ownerRename.map(rename => Found(owner, rename, prefixAfterRename))
      lazy val configRenamesSearchResult =
        renameConfigMap.get(owner).flatMap { rename =>
          // if the rename is taken, we don't want to use it
          indexedCtx.findSymbolInLocalScope(rename) match
            case Some(symbols) => None
            case None => Some(Missing(owner, rename, prefixAfterRename))
        }
      currentRenamesSearchResult orElse configRenamesSearchResult
    }.nextOption()

  private def isAccessibleStatically(sym: Symbol): Boolean =
    sym.isStatic || // Java static
      sym.maybeOwner.ownersIterator.forall { s =>
        s.is(Package) || s.is(Module)
      }

  private def optionalRootPrefix(sym: Symbol): Text =
    // If the symbol has toplevel clash we need to prepend `_root_.` to the symbol to disambiguate
    // it from the local symbol. It is only required when we are computing text for text edit.
    if isTextEdit && indexedCtx.toplevelClashes(sym, inImportScope = false) then
      Str("_root_.")
    else
      Text()

  private def findRename(tp: NamedType): Option[Text] =
    val maybePrefixRename = findPrefixRename(tp.symbol.maybeOwner)
    maybePrefixRename.map {
      case res: Found => res.toPrefixText
      case res: Missing =>
        val importSel =
          if res.owner.name.decoded == res.rename then
            ImportSel.Direct(res.owner)
          else ImportSel.Rename(res.owner, res.rename)

        missingImports += importSel
        res.toPrefixText
    }

  override def toTextPrefixOf(tp: NamedType): Text = controlled {
    val maybeRenamedPrefix: Option[Text] = findRename(tp)
    def trimmedPrefix: Text =
      if !tp.designator.isInstanceOf[Symbol] && tp.typeSymbol == NoSymbol then
        super.toTextPrefixOf(tp)
      else
        indexedCtx.lookupSym(tp.symbol, Some(tp.prefix)) match
          case _ if indexedCtx.rename(tp.symbol).isDefined => Text()
          // symbol is missing and is accessible statically, we can import it and add proper prefix
          case Result.Missing if isAccessibleStatically(tp.symbol) =>
            missingImports += ImportSel.Direct(tp.symbol)
            Text()
          // the symbol is in scope, we can omit the prefix
          case Result.InScope => Text()
          // the symbol is in conflict, we have to include prefix to avoid ambiguity
          case Result.Conflict =>
            maybeRenamedPrefix.getOrElse(super.toTextPrefixOf(tp))
          case _ => super.toTextPrefixOf(tp)

    optionalRootPrefix(tp.symbol) ~ maybeRenamedPrefix.getOrElse(trimmedPrefix)
  }

  override protected def selectionString(tp: NamedType): String =
    indexedCtx.rename(tp.symbol) match
      case Some(value) =>
        foundRenames += tp.symbol -> value
        value
      case None => super.selectionString(tp)

  override def toText(tp: Type): Text =
    tp match
      case c: ConstantType => toText(c.value)
      case tp if tp.isError => super.toText(indexedCtx.ctx.definitions.AnyType)
      case _ => super.toText(tp)

  override def toTextSingleton(tp: SingletonType): Text =
    tp match
      case ConstantType(const) => toText(const)
      case _ => toTextRef(tp) ~ ".type"

  def tpe(tpe: Type): String =
    val dealiased = if tpe.isNamedTupleType then tpe.deepDealiasAndSimplify else tpe
    toText(dealiased).mkString(defaultWidth)

  def hoverSymbol(sym: Symbol, info: Type)(using Context): String =
    val typeSymbol = info.typeSymbol

    def shortTypeString: String = tpe(info)

    def ownerTypeString: String =
      typeSymbol.owner.fullNameBackticked

    def name: String = nameString(sym)

    sym match
      case p if p.is(Flags.Package) =>
        s"package ${p.fullNameBackticked}"
      case c if c.is(Flags.EnumVal) =>
        s"case $name: $shortTypeString"
      // enum
      case e if e.is(Flags.Enum) || sym.companionClass.is(Flags.Enum) =>
        s"enum $name: $ownerTypeString"
      /* Type cannot be shown on the right since it is already a type
       * let's instead use that space to show the full path.
       */
      case o if typeSymbol.is(Flags.Module) => // enum
        s"${keyString(o)} $name: $ownerTypeString"
      case m if m.is(Flags.Method) =>
        defaultMethodSignature(m, info)
      case _ =>
        val implicitKeyword =
          if sym.is(Flags.Implicit) then List("implicit") else Nil
        val finalKeyword = if sym.is(Flags.Final) then List("final") else Nil
        val keyOrEmpty = keyString(sym)
        val keyword =
          if keyOrEmpty.iterator.nonEmpty then List(keyOrEmpty) else Nil
        (implicitKeyword ::: finalKeyword ::: keyword ::: (s"$name:" :: shortTypeString :: Nil))
          .mkString(" ")
  end hoverSymbol

  def isImportedByDefault(sym: Symbol): Boolean =
    lazy val effectiveOwner = sym.effectiveOwner
    sym.isType && (effectiveOwner == defn.ScalaPackageClass || effectiveOwner == defn.ScalaPredefModuleClass)

  def completionSymbol(denotation: Denotation): String =
    val info = denotation.info.widenTermRefExpr
    val typeSymbol = info.typeSymbol
    val sym = denotation.symbol

    lazy val typeEffectiveOwner =
      if typeSymbol != NoSymbol then " " + fullNameString(typeSymbol.effectiveOwner)
      else " " + fullNameString(sym.effectiveOwner)

    if isImportedByDefault(sym) then typeEffectiveOwner
    else if sym.is(Flags.Package) || sym.isClass then " " + fullNameString(sym.effectiveOwner)
    else if sym.is(Flags.Module) || typeSymbol.is(Flags.Module) then typeEffectiveOwner
    else if sym.is(Flags.Method) then
      defaultMethodSignature(sym, info, onlyMethodParams = true)
    else if sym.isType then
      info match
        case TypeAlias(t) => " = " + tpe(t.resultType)
        case t => tpe(t.resultType)
    else tpe(info)
  end completionSymbol

  /** Compute method signature for the given (method) symbol.
   *
   *  @return shortened name for types or the type for terms e.g. "[A:
   *    Ordering](a: A, b: B): collection.mutable.Map[A, B]" ":
   *    collection.mutable.Map[A, B]" for no-arg method
   */
  def defaultMethodSignature(
      gsym: Symbol,
      gtpe: Type,
      onlyMethodParams: Boolean = false,
      additionalMods: List[String] = Nil
  ): String =
    val namess = gtpe.paramNamess
    val infoss = gtpe.paramInfoss
    val nameToInfo: Map[Name, Type] = namess.flatten.lazyZip(infoss.flatten).toMap

    val (methodParams, extParams) = splitExtensionParamss(gsym)
    val paramss = methodParams ++ extParams
    lazy val implicitParams: List[Symbol] =
      paramss.flatMap(params => params.filter(p => p.isOneOf(Flags.GivenOrImplicit)))

    lazy val implicitEvidenceParams: Set[Symbol] =
      implicitParams
        .filter(p => p.name.toString.startsWith(ContextBoundParamName.separator))
        .toSet

    lazy val implicitEvidencesByTypeParam: Map[Symbol, List[String]] =
      constructImplicitEvidencesByTypeParam(
        implicitEvidenceParams.toList
      )

    lazy val paramsDocs =
      symbolSearch.symbolDocumentation(gsym) match
        case Some(info) =>
          (info.typeParameters().nn.asScala ++ info.parameters().nn.asScala).toSeq
        case _ =>
          Seq.empty

    def label(paramss: List[List[Symbol]]) = {
      var index = 0
      paramss.flatMap { params =>
        val labels = params.flatMap { param =>
          // Don't show implicit evidence params
          // e.g.
          // from [A: Ordering](a: A, b: A)(implicit evidence$1: Ordering[A])
          // to   [A: Ordering](a: A, b: A): A
          val lab =
            if implicitEvidenceParams.contains(param) then Nil
            else
              paramLabel(
                param,
                implicitEvidencesByTypeParam,
                index,
                if !onlyMethodParams then paramsDocs else Seq.empty,
                nameToInfo
              ) :: Nil
          index += 1
          lab

        }
        // Remove empty params
        if labels.isEmpty then Nil
        else labels.iterator :: Nil
      }
    }.iterator
    val paramLabelss = label(methodParams)
    val extLabelss = label(extParams)

    val retType = gtpe.finalResultType
    val simplified = if retType.typeSymbol.isAliasType then retType else retType.deepDealiasAndSimplify
    val returnType = tpe(simplified)
    def extensionSignatureString =
      val extensionSignature = paramssString(extLabelss, extParams)
      if extParams.nonEmpty then
        extensionSignature.mkString("extension ", "", " ")
      else ""
    val paramssSignature = paramssString(paramLabelss, methodParams)
      .mkString("", "", s": ${returnType}")

    val flags = (gsym.flags & methodFlags)
    val flagsSeq =
      if !flags.isEmpty then
        val privateWithin =
          if gsym.privateWithin != NoSymbol then gsym.privateWithin.name.show
          else ""
        flags.flagStrings(privateWithin)
      else Nil
    val mods = (additionalMods ++ flagsSeq).distinct match
      case Nil => ""
      case xs => xs.mkString("", " ", " ")

    if onlyMethodParams then paramssSignature
    else
      // For Scala2 compatibility, show "this" instead of <init> for constructor
      val name = if gsym.isConstructor then StdNames.nme.this_ else gsym.name
      extensionSignatureString +
        s"${mods}def $name" +
        paramssSignature
  end defaultMethodSignature

  def defaultValueSignature(
      gsym: Symbol,
      gtpe: Type,
      additionalMods: List[String] = Nil
  ): String =
    val flags = (gsym.flags & methodFlags)
    val flagString =
      if !flags.isEmpty then
        val privateWithin =
          if gsym.privateWithin != NoSymbol then gsym.privateWithin.name.show
          else ""
        flags.flagStrings(privateWithin).mkString(" ") + " "
      else ""
    val mods =
      if additionalMods.isEmpty then flagString
      else additionalMods.mkString(" ") + " " + flagString
    val prefix = if gsym.is(Mutable) then "var" else "val"
    s"${mods}$prefix ${gsym.name.show}: ${tpe(gtpe)}"

  /*
   * Check if a method is an extension method and in that case separate the parameters
   * into 2 groups to make it possible to print extensions properly.
   */
  private def splitExtensionParamss(
      gsym: Symbol
  ): (List[List[Symbol]], List[List[Symbol]]) =

    def headHasFlag(params: List[Symbol], flag: Flags.Flag): Boolean =
      params match
        case sym :: _ => sym.is(flag)
        case _ => false
    def isUsingClause(params: List[Symbol]): Boolean =
      headHasFlag(params, Flags.Given)
    def isTypeParamClause(params: List[Symbol]): Boolean =
      headHasFlag(params, Flags.TypeParam)
    def isUsingOrTypeParamClause(params: List[Symbol]): Boolean =
      isUsingClause(params) || isTypeParamClause(params)

    val paramss =
      if gsym.rawParamss.length != 0 then gsym.rawParamss else gsym.paramSymss
    if gsym.is(Flags.ExtensionMethod) then
      val filteredParams =
        if gsym.name.isRightAssocOperatorName then
          val (leadingTyParamss, rest1) = paramss match
            case fst :: tail if isTypeParamClause(fst) => (List(fst), tail)
            case other => (List(), other)
          val (leadingUsing, rest2) = rest1.span(isUsingClause)
          val (rightTyParamss, rest3) = rest2.span(isTypeParamClause)
          val (rightParamss, rest4) = rest3.splitAt(1)
          val (leftParamss, rest5) = rest4.splitAt(1)
          val (trailingUsing, rest6) = rest5.span(isUsingClause)
          if leftParamss.nonEmpty then
            leadingTyParamss ::: leadingUsing ::: leftParamss ::: rightTyParamss ::: rightParamss ::: trailingUsing ::: rest6
          else paramss // it wasn't a binary operator, after all.
        else paramss
      val trailingParamss = filteredParams
        .dropWhile(isUsingOrTypeParamClause)
        .drop(1)

      val leadingParamss =
        filteredParams.take(paramss.length - trailingParamss.length)
      (trailingParamss, leadingParamss)
    else (paramss, Nil)
  end splitExtensionParamss

  private def paramssString(
      paramLabels: Iterator[Iterator[String]],
      paramss: List[List[Symbol]]
  )(using Context): Iterator[String] =
    paramLabels
      .zipAll(paramss, Nil, Nil)
      .map { case (params, syms) =>
        Params.paramsKind(syms) match
          case Params.Kind.TypeParameter if params.iterator.nonEmpty =>
            params.iterator.mkString("[", ", ", "]")
          case Params.Kind.Normal =>
            params.iterator.mkString("(", ", ", ")")
          case Params.Kind.Using if params.iterator.nonEmpty =>
            params.iterator.mkString(
              "(using ",
              ", ",
              ")"
            )
          case Params.Kind.Implicit if params.iterator.nonEmpty =>
            params.iterator.mkString(
              "(implicit ",
              ", ",
              ")"
            )
          case _ => ""
      }
  end paramssString

  /** Construct param (both value params and type params) label string (e.g.
   *  "param1: TypeOfParam", "A: Ordering") for the given parameter's symbol.
   */
  private def paramLabel(
      param: Symbol,
      implicitEvidences: Map[Symbol, List[String]],
      index: Int,
      defaultValues: => Seq[SymbolDocumentation],
      nameToInfo: Map[Name, Type]
  )(using ReportContext): String =
    val docInfo = defaultValues.lift(index)
    val rawKeywordName = nameString(param)
    val keywordName = docInfo match
      case Some(info) if rawKeywordName.startsWith("x$") =>
        info.displayName()
      case _ => rawKeywordName
    val info = nameToInfo
      .get(param.name)
      .flatMap { info =>
        // In some cases, paramInfo becomes `... & Nothing` (e.g. CompletionOverrideSuite#cake)
        // which is meaningless, in that case, fallback to param.info
        if info <:< defn.NothingType then None
        else Some(info)
      }
      .getOrElse(param.info)

    // use `nameToInfo.get(param.name)` instead of `param.info` because
    // param.info loses `asSeenFrom` information:
    // parameter `f` of `foreach[U](f: A => U)` in `new scala.Traversable[Int]` should be `foreach[U](f: Int => U)`
    val paramTypeString = tpe(info)
    if param.isTypeParam then
      // pretty context bounds
      // e.g. f[A](a: A, b: A)(implicit evidence$1: Ordering[A])
      // to   f[A: Ordering](a: A, b: A)(implicit evidence$1: Ordering[A])
      val bounds = implicitEvidences.getOrElse(param, Nil) match
        case Nil => ""
        case head :: Nil => s": $head"
        case many => many.mkString(": ", ": ", "")
      s"$keywordName$paramTypeString$bounds"
    else if param.isAllOf(Given | Param) && param.name.startsWith("x$") then
      // For Anonymous Context Parameters
      // print only type string
      // e.g. "using Ord[T]" instead of "using x$0: Ord[T]"
      paramTypeString
    else
      val isDefaultParam = param.isAllOf(DefaultParameter)
      val default =
        if includeDefaultParam == ShortenedTypePrinter.IncludeDefaultParam.Include && isDefaultParam
        then
          val defaultValue = docInfo match
            case Some(value) if !value.defaultValue().nn.isEmpty() =>
              value.defaultValue()
            case _ => "..."
          s" = $defaultValue"
        // to be populated later, otherwise we would spend too much time in completions
        else if includeDefaultParam == ShortenedTypePrinter.IncludeDefaultParam.ResolveLater && isDefaultParam
        then " = ..."
        else "" // includeDefaultParam == Never or !isDefaultParam
      val inline = if param.is(Flags.Inline) then "inline " else ""
      s"$inline$keywordName: ${paramTypeString}$default"

  /** Create a mapping from type parameter symbol to its context bound string
   *  representations.
   *
   *  @param implicitEvidenceParams - implicit evidence params (e.g. evidence$1:
   *    Ordering[A])
   *  @return mapping from type param to its context bounds (e.g. Map(A ->
   *    List("Ordering")) )
   */
  private def constructImplicitEvidencesByTypeParam(
      implicitEvidenceParams: List[Symbol]
  ): Map[Symbol, List[String]] =
    val result = mutable.Map.empty[Symbol, mutable.ListBuffer[String]]
    implicitEvidenceParams.iterator
      .map(_.info)
      .collect {
        // AppliedType(TypeRef(ThisType(TypeRef(NoPrefix,module class reflect)),trait ClassTag),List(TypeRef(NoPrefix,type T)))
        case AppliedType(tycon, TypeRef(_, tparam) :: Nil)
            if tparam.isInstanceOf[Symbol] =>
          (tycon, tparam.asInstanceOf[Symbol])
      }
      .foreach { case (tycon, tparam) =>
        val buf =
          result.getOrElseUpdate(tparam, mutable.ListBuffer.empty[String])
        buf += tpe(tycon)
      }
    result.map(kv => (kv._1, kv._2.toList)).toMap
end ShortenedTypePrinter

object ShortenedTypePrinter:

  enum IncludeDefaultParam:
    /** Include default param at `textDocument/completion` */
    case Include

    /** Include default param as "..." and populate it later at
     *  `completionItem/resolve`
     *  @see
     *    https://github.com/scalameta/metals/blob/09d62c2e2f77a63c7d21ffa19971e2bb3fc9ab34/mtags/src/main/scala/scala/meta/internal/pc/ItemResolver.scala#L88-L103
     */
    case ResolveLater

    /** Do not add default parameter */
    case Never

end ShortenedTypePrinter
