package dotty.tools.pc
package completions

import java.nio.file.Path
import java.nio.file.Paths

import scala.collection.mutable
import scala.meta.internal.mtags.CoursierComplete
import scala.meta.internal.pc.{CompletionFuzzy, IdentifierComparator, MemberOrdering}
import scala.meta.pc.*
import scala.meta.pc.reports.ReportContext

import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Comments.Comment
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Decorators.toTermName
import dotty.tools.dotc.core.Denotations.SingleDenotation
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.NameOps.*
import dotty.tools.dotc.core.Names.*
import dotty.tools.dotc.core.StdNames
import dotty.tools.dotc.core.StdNames.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.interactive.Completion
import dotty.tools.dotc.interactive.Completion.Mode
import dotty.tools.dotc.util.SourcePosition
import dotty.tools.dotc.util.SrcPos
import dotty.tools.pc.AutoImports.AutoImportsGenerator
import dotty.tools.pc.buildinfo.BuildInfo
import dotty.tools.pc.completions.OverrideCompletions.OverrideExtractor
import dotty.tools.pc.utils.InteractiveEnrichments.*

class Completions(
    text: String,
    ctx: Context,
    search: SymbolSearch,
    buildTargetIdentifier: String,
    completionPos: CompletionPos,
    indexedContext: IndexedContext,
    path: List[Tree],
    adjustedPath: List[untpd.Tree],
    config: PresentationCompilerConfig,
    workspace: Option[Path],
    autoImports: AutoImportsGenerator,
    comments: List[Comment],
    options: List[String],
    completionItemPriority: CompletionItemPriority
)(using ReportContext):

  given context: Context = ctx

  private lazy val coursierComplete = new CoursierComplete(BuildInfo.scalaVersion)
  private lazy val completionMode = Completion.completionMode(adjustedPath, completionPos.originalCursorPosition)

  private lazy val shouldAddSnippet =
    path match
      case (_: (Import | Export)) :: _ => false
      case _ :: (_: (Import | Export)) :: _ => false
      // UnApply has patterns included in MatchCaseCompletions
      case _ :: (_: UnApply) :: _ => false
      case _ => true

  private lazy val isContinuedApply = path match
    /* In case of `method@@()` we should not add snippets and the path
     * will contain apply as the parent of the current tree.
     */
    case (fun) :: (appl: GenericApply) :: _ if appl.fun == fun => false
    /* In case of `T@@[]` we should not add snippets.
     */
    case tpe :: (appl: AppliedTypeTree) :: _ if appl.tpt == tpe => false
    case sel :: (funSel @ Select(fun, name)) :: (appl: GenericApply) :: _
        if appl.fun == funSel && sel == fun => false
    case _ => true

  private lazy val isDerivingTemplate = adjustedPath match
    /* In case of `class X derives TC@@` we shouldn't add `[]`
     */
    case Ident(_) :: (templ: untpd.DerivingTemplate) :: _ =>
      val pos = completionPos.toSourcePosition
      !templ.derived.exists(_.sourcePos.contains(pos))
    case _ => true

  private lazy val shouldAddSuffix = shouldAddSnippet && isContinuedApply && isDerivingTemplate

  private lazy val isNew: Boolean = Completion.isInNewContext(adjustedPath)

  def includeSymbol(sym: Symbol)(using Context): Boolean =
    def hasSyntheticCursorSuffix: Boolean =
      if !sym.name.endsWith(Cursor.value) then false
      else
        val realNameLength = sym.decodedName.length() - Cursor.value.length()
        sym.source == completionPos.originalCursorPosition.source &&
        sym.span.start + realNameLength == completionPos.queryEnd

    val generalExclude =
      isUninterestingSymbol(sym) ||
        !isNotLocalForwardReference(sym) ||
        hasSyntheticCursorSuffix

    def isWildcardParam(sym: Symbol) =
      if sym.isTerm && sym.owner.isAnonymousFunction then
        sym.name match
          case DerivedName(under, _) =>
            under.isEmpty
          case _ => false
      else false

    if generalExclude then false
    else if completionMode.is(Mode.Type) then true
    else !isWildcardParam(sym) && (sym.isTerm || sym.is(Package))
  end includeSymbol

  lazy val fuzzyMatcher: Name => Boolean = name =>
    if completionMode.is(Mode.Member) then CompletionFuzzy.matchesSubCharacters(completionPos.query, name.toString)
    else CompletionFuzzy.matches(completionPos.query, name.toString)

  def enrichedCompilerCompletions(qualType: Type): (List[CompletionValue], SymbolSearch.Result) =
    val compilerCompletions = Completion
      .rawCompletions(
        completionPos.originalCursorPosition,
        completionMode,
        completionPos.query,
        path,
        adjustedPath,
        Some(fuzzyMatcher)
      )

    compilerCompletions
      .toList
      .flatMap(toCompletionValues)
      .filterInteresting(qualType)

  def completions(): (List[CompletionValue], SymbolSearch.Result) =
    val (advanced, exclusive) = advancedCompletions(path, completionPos)
    val (all, result) =
      if exclusive then (advanced, SymbolSearch.Result.COMPLETE)
      else
        val keywords = KeywordsCompletions.contribute(path, completionPos, comments)
        val allAdvanced = advanced ++ keywords

        path match
          // should not show completions for toplevel
          case Nil | (_: PackageDef) :: _ if !completionPos.originalCursorPosition.source.file.ext.isScalaScript =>
            (allAdvanced, SymbolSearch.Result.COMPLETE)
          case Select(qual, _) :: _ if qual.typeOpt.isErroneous =>
            (allAdvanced, SymbolSearch.Result.COMPLETE)
          case Select(qual, _) :: _ =>
            val (compiler, result) = enrichedCompilerCompletions(qual.typeOpt.widenDealias)
            (allAdvanced ++ compiler, result)
          case _ =>
            val (compiler, result) = enrichedCompilerCompletions(defn.AnyType)
            (allAdvanced ++ compiler, result)

    val application = CompletionApplication.fromPath(path)
    val ordering = completionOrdering(application)
    val sorted = all.sorted(using ordering)
    val values = application.postProcess(sorted)
    (values, result)
  end completions

  private def toCompletionValues(
      completion: Name,
      denots: Seq[SingleDenotation]
  ): List[CompletionValue] =
    denots.toList.flatMap: denot =>
      completionsWithAffix(
        denot,
        completion.show,
        (label, denot, suffix) => CompletionValue.Compiler(label, denot, suffix)
      )
  end toCompletionValues

  inline private def undoBacktick(label: String): String =
    label.stripPrefix("`").stripSuffix("`")

  // TODO This has to be refactored to properly split extension methods
  // This method has to be fixed even further. The similar problem will be present in shortened type printer.
  private def getParams(symbol: Symbol) =
    lazy val extensionParam = symbol.extensionParam
    if symbol.is(Flags.Extension) then
      symbol.paramSymss.filterNot(
        _.contains(extensionParam)
      )
    else if symbol.isConstructor then
      symbol.owner.paramSymss
    else symbol.paramSymss.filter(!_.exists(_.isTypeParam))

  private def isAbstractType(symbol: Symbol) =
    (symbol.info.typeSymbol.is(Trait) // trait A{ def doSomething: Int}
      // object B{ new A@@}
      // Note: I realised that the value of Flag.Trait is flaky and
      // leads to the failure of one of the DocSuite tests
      || symbol.info.typeSymbol.isAllOf(
        Flags.JavaInterface // in Java:  interface A {}
          // in Scala 3: object B { new A@@}
      ) || symbol.info.typeSymbol.isAllOf(
        Flags.PureInterface // in Java: abstract class Shape { abstract void draw();}
          // Shape has only abstract members, so can be represented by a Java interface
          // in Scala 3: object B{ new Shap@@ }
      ) || (symbol.info.typeSymbol.is(Flags.Abstract) &&
        symbol.isClass) // so as to exclude abstract methods
      // abstract class A(i: Int){ def doSomething: Int}
      // object B{ new A@@}
    )
  end isAbstractType

  private def findSuffix(symbol: Symbol, adjustedPath: List[untpd.Tree]): CompletionAffix =
    CompletionAffix.empty
      .chain { suffix => // for [] suffix
        if shouldAddSuffix && symbol.info.typeParams.nonEmpty then
          suffix.withNewSuffixSnippet(Affix(SuffixKind.Bracket))
        else suffix
      }
      .chain { suffix =>
        adjustedPath match
          case (ident: Ident) :: (app @ Apply(_, List(arg))) :: _ =>
            app.symbol.info match
              case mt @ MethodType(termNames)
                  if app.symbol.paramSymss.last.exists(_.is(Given)) &&
                    !text.substring(app.fun.span.start, arg.span.end).contains("using") =>
                suffix.withNewPrefix(Affix(PrefixKind.Using))
              case _ => suffix
          case _ => suffix

      }
      .chain { suffix => // for () suffix
        if shouldAddSuffix && symbol.is(Flags.Method) then
          val paramss = getParams(symbol)
          paramss match
            case Nil => suffix
            case List(Nil) => suffix.withNewSuffix(Affix(SuffixKind.Brace))
            case _ if config.isCompletionSnippetsEnabled() =>
              val onlyParameterless = paramss.forall(_.isEmpty)
              lazy val onlyImplicitOrTypeParams = paramss.forall(
                _.exists { sym =>
                  sym.isType || sym.is(Implicit) || sym.is(Given)
                }
              )
              if onlyParameterless then suffix.withNewSuffix(Affix(SuffixKind.Brace))
              else if onlyImplicitOrTypeParams then suffix
              else if suffix.hasSnippet then suffix.withNewSuffix(Affix(SuffixKind.Brace))
              else suffix.withNewSuffixSnippet(Affix(SuffixKind.Brace))
            case _ => suffix
        else suffix
      }
      .chain { suffix => // for {} suffix
        if shouldAddSuffix && isNew && isAbstractType(symbol) then
          if suffix.hasSnippet then suffix.withNewSuffix(Affix(SuffixKind.Template))
          else suffix.withNewSuffixSnippet(Affix(SuffixKind.Template))
        else suffix
      }

  end findSuffix

  def completionsWithAffix(
      denot: SingleDenotation,
      label: String,
      toCompletionValue: (String, SingleDenotation, CompletionAffix) => CompletionValue.Symbolic
  ): List[CompletionValue] =
    val sym = denot.symbol
    val hasNonSyntheticConstructor = sym.name.isTypeName && sym.isClass
      && !sym.is(ModuleClass) && !sym.is(Trait) && !sym.is(Abstract) && !sym.is(Flags.JavaDefined)

    val (extraMethodDenots, skipOriginalDenot): (List[SingleDenotation], Boolean) =
      if shouldAddSnippet && isNew && hasNonSyntheticConstructor then
        val constructors = sym.info.member(nme.CONSTRUCTOR).allSymbols.map(_.asSingleDenotation)
          .filter(_.symbol.isAccessibleFrom(denot.info))
        constructors -> true
      else if shouldAddSnippet && completionMode.is(Mode.Term) && sym.name.isTermName &&
        !sym.is(
          Flags.JavaDefined
        ) && (sym.isClass || sym.is(Module) || (sym.isField && denot.info.isInstanceOf[TermRef]))
      then

        val constructors = if sym.isAllOf(ConstructorProxyModule) then
          sym.companionClass.info.member(nme.CONSTRUCTOR).allSymbols
        else
          val companionApplies = denot.info.member(nme.apply).allSymbols
          val classConstructors = if sym.companionClass.exists && !sym.companionClass.isOneOf(AbstractOrTrait) then
            sym.companionClass.info.member(nme.CONSTRUCTOR).allSymbols
          else Nil

          if companionApplies.exists(_.is(Synthetic)) then
            companionApplies ++ classConstructors.filter(!_.isPrimaryConstructor)
          else
            companionApplies ++ classConstructors

        val result = constructors.map(_.asSeenFrom(denot.info).asSingleDenotation)
          .filter(_.symbol.isAccessibleFrom(denot.info))

        result -> (sym.isAllOf(ConstructorProxyModule) || sym.is(Trait))
      else Nil -> false

    val extraCompletionValues =
      val existsApply = extraMethodDenots.exists(_.symbol.name == nme.apply)

      extraMethodDenots.map { methodDenot =>
        val suffix = findSuffix(methodDenot.symbol, adjustedPath)
        val affix = if methodDenot.symbol.isConstructor && existsApply then
          adjustedPath match
            case (select @ Select(qual, _)) :: _ =>
              val insertRange = select.sourcePos.startPos.withEnd(completionPos.queryEnd).toLsp

              suffix
                .withCurrentPrefix(qual.show + ".")
                .withNewPrefix(Affix(PrefixKind.New, insertRange = Some(insertRange)))
            case _ =>
              suffix.withNewPrefix(Affix(PrefixKind.New))
        else suffix
        val name = undoBacktick(label)

        CompletionValue.ExtraMethod(
          owner = denot,
          extraMethod = toCompletionValue(name, methodDenot, affix)
        )
      }

    if skipOriginalDenot then extraCompletionValues
    else
      val suffix = findSuffix(denot.symbol, adjustedPath)
      val name = undoBacktick(label)
      val denotCompletionValue = toCompletionValue(name, denot, suffix)
      denotCompletionValue :: extraCompletionValues

  end completionsWithAffix

  /** @return Tuple of completionValues and flag. If the latter boolean value is
   *    true Metals should provide advanced completions only.
   */
  private def advancedCompletions(
      path: List[Tree],
      completionPos: CompletionPos
  ): (List[CompletionValue], Boolean) =
    val pos = completionPos.originalCursorPosition
    lazy val rawPath = Paths
      .get(pos.source.path).nn
    lazy val rawFileName = rawPath
      .getFileName()
      .toString()
    lazy val filename = rawFileName
      .stripSuffix(".scala")
    val MatchCaseExtractor = new MatchCaseExtractor(pos, text, completionPos)
    val ScalaCliCompletions =
      new ScalaCliCompletions(coursierComplete, pos, text)

    val (advanced, exclusive) = path match
      case ScalaCliCompletions(dependency) =>
        (ScalaCliCompletions.contribute(dependency), true)

      case _
          if MultilineCommentCompletion.isMultilineCommentCompletion(
            pos,
            text
          ) =>
        val values = MultilineCommentCompletion.contribute(config)
        (values, true)

      case _ if ScaladocCompletions.isScaladocCompletion(pos, text) =>
        val values = ScaladocCompletions.contribute(pos, text, config)
        (values, true)

      case MatchCaseExtractor.MatchExtractor(selector) =>
        (
          CaseKeywordCompletion.matchContribute(
            selector,
            completionPos,
            indexedContext,
            config,
            search,
            autoImports,
            options.contains("-no-indent")
          ),
          false
        )

      case MatchCaseExtractor.TypedCasePatternExtractor(
            selector,
            parent,
            identName
          ) =>
        (
          CaseKeywordCompletion.contribute(
            selector,
            completionPos,
            indexedContext,
            config,
            search,
            parent,
            autoImports,
            patternOnly = Some(identName),
            hasBind = true
          ),
          false
        )

      case MatchCaseExtractor.CasePatternExtractor(
            selector,
            parent,
            identName
          ) =>
        (
          CaseKeywordCompletion.contribute(
            selector,
            completionPos,
            indexedContext,
            config,
            search,
            parent,
            autoImports,
            patternOnly = Some(identName)
          ),
          false
        )

      case MatchCaseExtractor.CaseExtractor(
            selector,
            parent,
            includeExhaustive
          ) =>
        (
          CaseKeywordCompletion.contribute(
            selector,
            completionPos,
            indexedContext,
            config,
            search,
            parent,
            autoImports,
            includeExhaustive = includeExhaustive
          ),
          true
        )

      // unapply pattern
      case Ident(name) :: (unapp: UnApply) :: _ =>
        (
          CaseKeywordCompletion.contribute(
            EmptyTree, // no selector
            completionPos,
            indexedContext,
            config,
            search,
            parent = unapp,
            autoImports,
            patternOnly = Some(name.decoded)
          ),
          false
        )
      case Select(_, name) :: (unapp: UnApply) :: _ =>
        (
          CaseKeywordCompletion.contribute(
            EmptyTree, // no selector
            completionPos,
            indexedContext,
            config,
            search,
            parent = unapp,
            autoImports,
            patternOnly = Some(name.decoded)
          ),
          false
        )

      // class FooImpl extends Foo:
      //   def x|
      case OverrideExtractor(td, completing, start, exhaustive, fallbackName) =>
        (
          OverrideCompletions.contribute(
            td,
            completing,
            start,
            indexedContext,
            search,
            config,
            autoImports,
            fallbackName
          ),
          exhaustive
        )

      // class Fo@@
      case (td: TypeDef) :: _
          if CompletionFuzzy.matches(
            td.symbol.name.decoded.replace(Cursor.value, "").nn,
            filename
          ) =>
        val values = FilenameCompletions.contribute(filename, td)
        (values, true)
      case (lit @ Literal(Constant(_: String))) :: _ =>
        val completions = InterpolatorCompletions
          .contribute(
            text,
            completionPos,
            indexedContext,
            lit,
            path,
            this,
            config.isCompletionSnippetsEnabled(),
            search,
            config,
            buildTargetIdentifier
          )
          .filterInteresting(enrich = false)
          ._1
        (completions, true)

      case (imp @ Import(expr, selectors)) :: _
          if isAmmoniteCompletionPosition(imp, rawFileName, "$file") =>
        (
          AmmoniteFileCompletions.contribute(
            expr,
            selectors,
            pos.endPos.toLsp,
            rawPath.toString(),
            workspace,
            rawFileName
          ),
          true
        )

      case (imp @ Import(_, selectors)) :: _
          if isAmmoniteCompletionPosition(imp, rawFileName, "$ivy") ||
            isWorksheetIvyCompletionPosition(imp, imp.sourcePos) =>
        (
          AmmoniteIvyCompletions.contribute(
            coursierComplete,
            selectors,
            completionPos,
            text
          ),
          true
        )

      case (tree: (Import | Export)) :: _
          if tree.selectors.exists(_.renamed.sourcePos.contains(pos)) =>
        (List.empty, true)

      case _ =>
        val args = NamedArgCompletions.contribute(
          path,
          adjustedPath,
          indexedContext,
          config.isCompletionSnippetsEnabled()
        )
        (args, false)
    val singletonCompletions = InferCompletionType.inferType(path).map(
      SingletonCompletions.contribute(path, _, completionPos)
    ).getOrElse(Nil)
    (singletonCompletions ++ advanced, exclusive)
  end advancedCompletions

  private def isAmmoniteCompletionPosition(
      tree: Tree,
      fileName: String,
      magicImport: String
  ): Boolean =

    def getQualifierStart(identOrSelect: Tree): String =
      identOrSelect match
        case Ident(name) => name.toString
        case Select(newQual, name) => getQualifierStart(newQual)
        case _ => ""

    tree match
      case Import(identOrSelect, _) =>
        fileName.isAmmoniteGeneratedFile && getQualifierStart(identOrSelect)
          .toString()
          .startsWith(magicImport)
      case _ => false
  end isAmmoniteCompletionPosition

  def isWorksheetIvyCompletionPosition(
      tree: Tree,
      pos: SourcePosition
  ): Boolean =
    tree match
      case Import(Ident(ivy), _) =>
        pos.source.file.name.isWorksheet &&
        (ivy.decoded == "$ivy" ||
          ivy.decoded == "$dep")
      case _ => false

  private def enrichWithSymbolSearch(
      visit: CompletionValue => Boolean,
      qualType: Type = ctx.definitions.AnyType
  ): Option[SymbolSearch.Result] =
    val query = completionPos.query
    if completionMode.is(Mode.Scope) && query.nonEmpty then
      val visitor = new CompilerSearchVisitor(sym =>
        if Completion.isValidCompletionSymbol(sym, completionMode, isNew) &&
          !(sym.is(Flags.ExtensionMethod) || (sym.maybeOwner.is(Flags.Implicit) && sym.maybeOwner.isClass))
        then
          indexedContext.lookupSym(sym) match
            case IndexedContext.Result.InScope => false
            case IndexedContext.Result.Missing if indexedContext.rename(sym).isDefined => false
            case _ if completionMode.is(Mode.ImportOrExport) =>
              visit(
                CompletionValue.Workspace(
                  label = undoBacktick(sym.decodedName),
                  denotation = sym,
                  snippetAffix = CompletionAffix.empty,
                  importSymbol = sym
                )
              )
            case _ =>
              completionsWithAffix(
                sym,
                sym.decodedName,
                CompletionValue.Workspace(_, _, _, sym)
              ).map(visit).forall(_ == true)
        else false
      )
      Some(search.search(query, buildTargetIdentifier, visitor).nn)
    else if completionMode.is(Mode.Member) && query.nonEmpty then
      val visitor = new CompilerSearchVisitor(sym =>
        def isExtensionMethod = sym.is(ExtensionMethod) &&
          qualType.widenDealias <:< sym.extensionParam.info.widenDealias
        def isImplicitClass(owner: Symbol) =
          val constructorParam =
            owner.info
              .membersBasedOnFlags(
                Flags.ParamAccessor,
                Flags.EmptyFlags
              )
              .headOption
              .map(_.info)
          owner.isClass && owner.is(Flags.Implicit) &&
          constructorParam.exists(p =>
            qualType.widenDealias <:< p.widenDealias
          )

        def isDefaultVariableSetter = sym.is(Flags.Accessor) && sym.is(Flags.Method)
        def isImplicitClassMember =
          isImplicitClass(sym.maybeOwner) && !sym.is(Flags.Synthetic) && sym.isPublic
            && !sym.isConstructor && !isDefaultVariableSetter

        if isExtensionMethod then
          completionsWithAffix(
            sym,
            sym.decodedName,
            CompletionValue.Extension(_, _, _)
          ).map(visit).forall(_ == true)
        else if isImplicitClassMember then
          completionsWithAffix(
            sym,
            sym.decodedName,
            CompletionValue.ImplicitClass(_, _, _, sym.maybeOwner)
          ).map(visit).forall(_ == true)
        else false
      )
      Some(search.searchMethods(query, buildTargetIdentifier, visitor).nn)
    else Some(SymbolSearch.Result.INCOMPLETE)

  end enrichWithSymbolSearch

  extension (s: SrcPos)
    def isAfter(s1: SrcPos) =
      s.sourcePos.exists && s1.sourcePos.exists && s.sourcePos.point > s1.sourcePos.point

  extension (sym: Symbol)
    def detailString: String =
      if sym.is(Method) then
        val sig = sym.signature
        val sigString =
          if sig.paramsSig.isEmpty then "()"
          else
            sig.paramsSig
              .map(p => p.toString)
              .mkString("(", ",", ")")
        sym.showFullName + sigString
      else sym.fullName.stripModuleClassSuffix.show

  /** If we try to complete TypeName, we should favor types over terms with same
   *  name value and without suffix.
   */
  def deduplicateCompletions(completions: List[CompletionValue]): List[CompletionValue] =
    val (symbolicCompletions, rest) = completions.partition:
      _.isInstanceOf[CompletionValue.Symbolic]

    val symbolicCompletionsMap = symbolicCompletions
      .collect { case symbolic: CompletionValue.Symbolic => symbolic }
      .groupBy(_.symbol.fullName) // we somehow have to ignore proxy type

    val filteredSymbolicCompletions = symbolicCompletionsMap.filter: (name, _) =>
      lazy val existsTypeWithoutSuffix: Boolean = !symbolicCompletionsMap
        .get(name.toTypeName)
        .forall(_.forall(sym => sym.snippetAffix.suffixes.nonEmpty))

      (completionMode.is(Mode.Term) && !completionMode.is(Mode.ImportOrExport)) ||
      // show non synthetic symbols
      // companion test should not result TrieMap[K, V]
      (name.isTermName && !existsTypeWithoutSuffix) ||
      name.isTypeName
    .toList.unzip._2.flatten

    filteredSymbolicCompletions ++ rest

  extension (l: List[CompletionValue])
    def filterInteresting(
        qualType: Type = ctx.definitions.AnyType,
        enrich: Boolean = true
    ): (List[CompletionValue], SymbolSearch.Result) =
      val alreadySeen = mutable.Set.empty[String]
      val buf = List.newBuilder[CompletionValue]
      def visit(head: CompletionValue): Boolean =
        val (id, include) =
          head match
            case doc: CompletionValue.Document => (doc.label, true)
            case over: CompletionValue.Override => (over.label, true)
            case ck: CompletionValue.CaseKeyword => (ck.label, true)
            case symOnly: CompletionValue.Symbolic =>
              val sym = symOnly.symbol
              val name = symOnly match
                case CompletionValue.ExtraMethod(owner, extraMethod) =>
                  SemanticdbSymbols.symbolName(owner.symbol) + SemanticdbSymbols.symbolName(extraMethod.symbol)
                case _ => SemanticdbSymbols.symbolName(sym)
              val suffix =
                if symOnly.snippetAffix.addLabelSnippet then "[]" else ""
              val id = name + suffix
              val include = includeSymbol(sym)
              (id, include)
            case kw: CompletionValue.Keyword => (kw.label, true)
            case mc: CompletionValue.MatchCompletion => (mc.label, true)
            case autofill: CompletionValue.Autofill =>
              (autofill.label, true)
            case fileSysMember: CompletionValue.FileSystemMember =>
              (fileSysMember.label, true)
            case ii: CompletionValue.Coursier => (ii.label, true)
            case sv: CompletionValue.SingletonValue => (sv.label, true)

        if !alreadySeen(id) && include then
          alreadySeen += id
          buf += head
          true
        else false

      l.foreach(visit)

      if enrich then
        val searchResult =
          enrichWithSymbolSearch(visit, qualType).getOrElse(SymbolSearch.Result.COMPLETE)
        (deduplicateCompletions(buf.result()), searchResult)
      else (deduplicateCompletions(buf.result()), SymbolSearch.Result.COMPLETE)
    end filterInteresting
  end extension

  private lazy val isUninterestingSymbol: Set[Symbol] = Set[Symbol](
    defn.Any_==,
    defn.Any_!=,
    defn.Any_##,
    defn.Object_eq,
    defn.Object_ne,
    defn.RepeatedParamClass,
    defn.ByNameParamClass2x,
    defn.Object_notify,
    defn.Object_notifyAll,
    defn.Object_notify,
    defn.Predef_undefined,
    // NOTE(olafur) IntelliJ does not complete the root package and without this filter
    // then `_root_` would appear as a completion result in the code `foobar(_<COMPLETE>)`
    defn.RootPackage,
    // NOTE(gabro) valueOf was added as a Predef member in 2.13. We filter it out since is a niche
    // use case and it would appear upon typing 'val'
    defn.ValueOfClass
  ) ++ (
    Set(
      defn.ObjectClass.info.member(nme.wait_),
      defn.ScalaPredefModule.info.member(nme.valueOf)
    ).flatMap(_.alternatives.map(_.symbol)).toSet
  )

  private lazy val EqualsClass: ClassSymbol = requiredClass("scala.Equals")
  private lazy val ArrowAssocClass: ClassSymbol = requiredClass("scala.Predef.ArrowAssoc")
  private lazy val EnsuringClass: ClassSymbol = requiredClass("scala.Predef.Ensuring")
  private lazy val StringFormatClass: ClassSymbol = requiredClass("scala.Predef.StringFormat")
  private lazy val nnMethod: Symbol = defn.ScalaPredefModule.info.member("nn".toTermName).symbol
  private lazy val runtimeCheckedMethod: Symbol = defn.ScalaPredefModule.info.member("runtimeChecked".toTermName).symbol

  private def isNotLocalForwardReference(sym: Symbol)(using Context): Boolean =
    !sym.isLocalToBlock ||
      !sym.srcPos.isAfter(completionPos.originalCursorPosition) ||
      sym.is(Param)

  private def computeRelevancePenalty(
      completion: CompletionValue,
      application: CompletionApplication
  ): Int =
    import scala.meta.internal.pc.MemberOrdering.*

    def hasGetter(sym: Symbol) =
      try
        def isModuleOrClass = sym.is(Module) || sym.isClass
        // isField returns true for some classes
        def isJavaClass = sym.is(JavaDefined) && isModuleOrClass
        (sym.isField && !isJavaClass && !isModuleOrClass) || sym.getter != NoSymbol
      catch case _ => false

    def isInheritedFromScalaLibrary(sym: Symbol) =
      sym.owner == defn.AnyClass ||
        sym.owner == defn.ObjectClass ||
        sym.owner == defn.ProductClass ||
        sym.owner == EqualsClass ||
        sym.owner == ArrowAssocClass ||
        sym.owner == EnsuringClass ||
        sym.owner == StringFormatClass ||
        sym == nnMethod ||
        sym == runtimeCheckedMethod

    def symbolRelevance(sym: Symbol): Int =
      var relevance = 0
      // symbols defined in this file are more relevant
      if completionPos.originalCursorPosition.source != sym.source || sym.is(Package) then
        relevance |= IsNotDefinedInFile

      // fields are more relevant than non fields (such as method)
      completion match
        // For override-completion, we don't care fields or methods because
        // we can override both fields and non-fields
        case _: CompletionValue.Override =>
          relevance |= IsNotGetter
        case _ if !hasGetter(sym) =>
          relevance |= IsNotGetter
        case _ =>

      // symbols whose owner is a base class are less relevant
      if isInheritedFromScalaLibrary(sym)
      then relevance |= IsInheritedBaseMethod
      // symbols not provided via an implicit are more relevant
      if sym.is(Implicit) ||
        sym.is(ExtensionMethod) ||
        application.isImplicitConversion(sym)
      then relevance |= IsImplicitConversion
      if application.isInherited(sym) then relevance |= IsInherited
      if sym.is(Package) then relevance |= IsPackage
      // accessors of case class members are more relevant
      if !sym.is(CaseAccessor) then relevance |= IsNotCaseAccessor
      // public symbols are more relevant
      if !sym.isPublic then relevance |= IsNotPublic
      // synthetic symbols are less relevant (e.g. `copy` on case classes)
      if sym.is(Synthetic) && !sym.isAllOf(EnumCase) then
        relevance |= IsSynthetic
      if sym.isDeprecated then relevance |= IsDeprecated
      if isEvilMethod(sym.name) then relevance |= IsEvilMethod
      if !completionMode.is(Mode.ImportOrExport) &&
        completionMode.is(Mode.Type) && !sym.isType
      then relevance |= IsNotTypeInTypePos
      relevance
    end symbolRelevance

    def computeRelevance(sym: Symbol, completionValue: CompletionValue.Symbolic) =
      completionValue match
        case _: CompletionValue.Override =>
          var penalty = symbolRelevance(sym)
          // show the abstract members first
          if !sym.is(Deferred) then penalty |= MemberOrdering.IsNotAbstract
          penalty
        case _: CompletionValue.Workspace =>
          symbolRelevance(sym) | (IsWorkspaceSymbol + sym.name.show.length())
        case _ => symbolRelevance(sym)

    completion match
      case CompletionValue.ExtraMethod(owner, extraMethod) =>
        computeRelevance(owner.symbol, extraMethod)
      case sym: CompletionValue.Symbolic =>
        computeRelevance(sym.symbol, sym)
      case _ => Int.MaxValue

  end computeRelevancePenalty

  private lazy val isEvilMethod: Set[Name] = Set[Name](
    nme.notifyAll_,
    nme.notify_,
    nme.wait_,
    nme.clone_,
    nme.finalize_
  )

  trait CompletionApplication:
    def isImplicitConversion(symbol: Symbol): Boolean
    def isMember(symbol: Symbol): Boolean
    def isInherited(symbol: Symbol): Boolean
    def postProcess(items: List[CompletionValue]): List[CompletionValue]

  object CompletionApplication:
    val empty = new CompletionApplication:
      def isImplicitConversion(symbol: Symbol): Boolean = false
      def isMember(symbol: Symbol): Boolean = false
      def isInherited(symbol: Symbol): Boolean = false
      def postProcess(items: List[CompletionValue]): List[CompletionValue] =
        items

    def forSelect(sel: Select): CompletionApplication =
      val tpe = sel.qualifier.typeOpt
      val members = tpe.allMembers.map(_.symbol).toSet

      new CompletionApplication:
        def isImplicitConversion(symbol: Symbol): Boolean =
          !isMember(symbol)
        def isMember(symbol: Symbol): Boolean = members.contains(symbol)
        def isInherited(symbol: Symbol): Boolean =
          isMember(symbol) && symbol.owner != tpe.typeSymbol
        def postProcess(items: List[CompletionValue]): List[CompletionValue] =
          items.map {
            case completion @ CompletionValue.Compiler(label, denot, suffix)
                if isMember(completion.symbol) =>
              CompletionValue.Compiler(
                label,
                substituteTypeVars(completion.symbol),
                suffix
              )
            case other => other
          }

        private def substituteTypeVars(symbol: Symbol): Symbol =
          val denot = symbol.asSeenFrom(tpe)
          symbol.withUpdatedTpe(denot.info)
    end forSelect

    def fromPath(path: List[Tree]): CompletionApplication =
      path.headOption match
        case Some(Select(qual @ This(_), _)) if qual.span.isSynthetic => empty
        case Some(select: Select) => forSelect(select)
        case _ => empty

  end CompletionApplication

  private def completionOrdering(
      application: CompletionApplication
  ): Ordering[CompletionValue] =
    new Ordering[CompletionValue]:
      val queryLower: String = completionPos.query.toLowerCase()
      val fuzzyCache = mutable.Map.empty[CompletionValue, Int]

      def compareLocalSymbols(s1: Symbol, s2: Symbol): Int =
        if s1.isLocal && s2.isLocal && s1.sourcePos.exists && s2.sourcePos.exists
        then
          val firstIsAfter = s1.srcPos.isAfter(s2.srcPos)
          if firstIsAfter then -1 else 1
        else 0
      end compareLocalSymbols

      private def workspaceMemberPriority(symbol: Symbol): Int =
        completionItemPriority
          .workspaceMemberPriority(
            SemanticdbSymbols.symbolName(symbol)
          ).nn

      def compareFrequency(o1: CompletionValue, o2: CompletionValue): Int =
        (o1, o2) match
          case (w1: CompletionValue.Workspace, w2: CompletionValue.Workspace) =>
            workspaceMemberPriority(w1.symbol)
              .compareTo(workspaceMemberPriority(w2.symbol))
          case _ => 0
      end compareFrequency

      def compareByRelevance(o1: CompletionValue, o2: CompletionValue): Int =
        Integer.compare(
          computeRelevancePenalty(o1, application),
          computeRelevancePenalty(o2, application)
        )

      def fuzzyScore(o: CompletionValue.Symbolic): Int =
        fuzzyCache.getOrElseUpdate(
          o, {
            val name = o.label.toLowerCase().nn
            if name.startsWith(queryLower) then 0
            else if name.contains(queryLower) then 1
            else 2
          }
        )
      def prioritizeByClass(o1: CompletionValue, o2: CompletionValue): Int =
        def priority(v: CompletionValue): Int =
          v match
            case _: CompletionValue.SingletonValue => 0
            case _: CompletionValue.Compiler => 1
            case CompletionValue.ExtraMethod(_, _: CompletionValue.Compiler) => 1
            case _: CompletionValue.CaseKeyword => 2
            case _: CompletionValue.NamedArg => 3
            case _: CompletionValue.Keyword => 4
            case _ => 5

        priority(o1) - priority(o2)

      /** Some completion values should be shown first such as CaseKeyword and
       *  NamedArg
       */
      def compareCompletionValue(
          sym1: CompletionValue.Symbolic,
          sym2: CompletionValue.Symbolic
      ): Boolean =
        val prioritizeCaseKeyword =
          sym1.isInstanceOf[CompletionValue.CaseKeyword] &&
            !sym2.isInstanceOf[CompletionValue.CaseKeyword]

        // if the name is the same as the parameter name then we should show the symbolic first
        val prefixMatches =
          sym1.symbol.name.toString().startsWith(sym2.symbol.name.toString())

        val prioritizeNamed =
          sym1.isInstanceOf[CompletionValue.NamedArg] &&
            !sym2.isInstanceOf[CompletionValue.NamedArg] &&
            !prefixMatches

        prioritizeCaseKeyword || prioritizeNamed
      end compareCompletionValue

      def methodScore(v: CompletionValue.Symbolic)(using Context): Int =
        val sym = v.symbol
        val workspacePenalty = v match
          case CompletionValue.ExtraMethod(_, _: CompletionValue.Workspace) => 5
          case _: CompletionValue.Workspace => 5
          case _ => 0

        val isExtraMethod = v.isInstanceOf[CompletionValue.ExtraMethod]
        val methodPenalty =
          if isNew && sym.isConstructor then -1
          else if isExtraMethod && !sym.isConstructor then 1
          else if isExtraMethod then 2
          else if !sym.isAllOf(SyntheticModule) then 3
          else 4

        workspacePenalty + methodPenalty

      override def compare(o1: CompletionValue, o2: CompletionValue): Int =
        (o1, o2) match
          case (o1: CompletionValue.NamedArg, o2: CompletionValue.NamedArg) =>
            IdentifierComparator.compare(
              o1.label,
              o2.label
            )
          case (sym1: CompletionValue.Symbolic, sym2: CompletionValue.Symbolic) =>
            if compareCompletionValue(sym1, sym2) then 0
            else if compareCompletionValue(sym2, sym1) then 1
            else
              val s1 = sym1.symbol
              val s2 = sym2.symbol
              val byLocalSymbol = compareLocalSymbols(s1, s2)
              if byLocalSymbol != 0 then byLocalSymbol
              else
                val byFuzzy = Integer.compare(
                  fuzzyScore(sym1),
                  fuzzyScore(sym2)
                )
                if byFuzzy != 0 then byFuzzy
                else
                  val byRelevance = compareByRelevance(o1, o2)
                  if byRelevance != 0 then byRelevance
                  else
                    val byMethodScore = Integer.compare(
                      methodScore(sym1),
                      methodScore(sym2)
                    )
                    if byMethodScore != 0 then byMethodScore
                    else
                      val byIdentifier = IdentifierComparator.compare(
                        s1.name.show,
                        s2.name.show
                      )
                      if byIdentifier != 0 then byIdentifier
                      else
                        val byFrequency = compareFrequency(o1, o2)
                        if byFrequency != 0 then byFrequency
                        else
                          val byOwner =
                            s1.owner.fullName.toString
                              .compareTo(s2.owner.fullName.toString)
                          if byOwner != 0 then byOwner
                          else
                            val byParamCount = Integer.compare(
                              s1.paramSymss.flatten.size,
                              s2.paramSymss.flatten.size
                            )
                            if byParamCount != 0 then byParamCount
                            else s1.detailString.compareTo(s2.detailString)
            end if
          case (sym1: CompletionValue.Coursier, sym2: CompletionValue.Coursier) =>
            val comparison = IdentifierComparator.compare(sym1.label, sym2.label)
            if sym1.isVersionCompletion && sym2.isVersionCompletion then -comparison
            else comparison
          case _ =>
            val byClass = prioritizeByClass(o1, o2)
            if byClass != 0 then byClass
            else compareByRelevance(o1, o2)
      end compare

end Completions
