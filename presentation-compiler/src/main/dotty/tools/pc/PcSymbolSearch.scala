package dotty.tools.pc

import dotty.tools.dotc.ast.NavigateAST
import dotty.tools.dotc.ast.Positioned
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.ast.untpd.ExtMethods
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.NameOps.*
import dotty.tools.dotc.core.Names.*
import dotty.tools.dotc.core.StdNames.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.interactive.Interactive
import dotty.tools.dotc.util.SourcePosition
import dotty.tools.dotc.util.Spans.{spanCoord, Span}
import dotty.tools.pc.PcSymbolSearch.*
import dotty.tools.pc.utils.InteractiveEnrichments.*

trait PcSymbolSearch:
  self: WithCompilationUnit =>

  private val caseClassSynthetics: Set[Name] = Set(nme.apply, nme.copy)

  lazy val rawPath =
    Interactive
      .pathTo(driver.openedTrees(uri), pos)(using driver.currentCtx)
      .dropWhile(t => // NamedArg anyway doesn't have symbol
        t.symbol == NoSymbol && !t.isInstanceOf[NamedArg] ||
          // same issue https://github.com/lampepfl/dotty/issues/15937 as below
          t.isInstanceOf[TypeTree]
      )

  lazy val extensionMethods =
    NavigateAST
      .untypedPath(pos.span)(using compilatonUnitContext)
      .collectFirst { case em @ ExtMethods(_, _) => em }

  lazy val path = rawPath match
    /* The `NamedTuple.apply` reference synthesized for a named tuple selection `x.name`
     * carries the span of the qualifier `x`, so `pathTo` stops at it when the cursor
     * is on the qualifier. Redirect into the qualifier instead.
     */
    case (sel: Select) :: (_: TypeApply) :: Apply(_, List(qual)) :: rest
        if isNamedTupleApply(sel.symbol) && qual.span.contains(pos.span) =>
      namedTupleQualifierPath(qual) ::: rest
    // For type it will sometimes go into the wrong tree since TypeTree also contains the same span
    // https://github.com/lampepfl/dotty/issues/15937
    case TypeApply(sel: Select, _) :: _ if sel.span.contains(pos.span) =>
      Interactive.pathTo(sel, pos.span) ::: rawPath
    case _ => rawPath

  /** Path into the qualifier of a named tuple selection, skipping the synthetic
   *  `NamedTuple.apply` references of nested selections such as `x.inner.name`.
   */
  private def namedTupleQualifierPath(qual: Tree): List[Tree] =
    Interactive.pathTo(qual, pos.span) match
      case (sel: Select) :: (_: TypeApply) :: Apply(_, List(innerQual)) :: rest
          if isNamedTupleApply(sel.symbol) && innerQual.span.contains(pos.span) =>
        namedTupleQualifierPath(innerQual) ::: rest
      case path => path

  /** Whether the cursor is on the field of a named tuple selection (`x.na@@me`).
   *  Such fields have no declaration symbol: they can be highlighted and searched
   *  for, but not renamed.
   */
  lazy val isOnNamedTupleField: Boolean = path match
    case (app @ NamedTupleSelection(_, _, _, _)) :: _ =>
      namedTupleFieldSpan(app).contains(pos.span)
    case _ => false

  lazy val soughtSymbols: Option[(Set[Symbol], SourcePosition)] =
    soughtSymbols(path)

  private def soughtSymbols(path: List[Tree]): Option[(Set[Symbol], SourcePosition)] =
    val sought = path match
      /* reference of an extension paramter
       * extension [EF](<<xs>>: List[EF])
       *   def double(ys: List[EF]) = <<x@@s>> ++ ys
       */
      case (id: Ident) :: _
          if id.symbol
            .is(Flags.Param) && id.symbol.owner.is(Flags.ExtensionMethod) =>
        Some(findAllExtensionParamSymbols(id.sourcePos, id.name, id.symbol))

      /** Workaround for missing symbol in:
       *  ```
       *  class A[T](a: T)
       *  val x = new <<A>>(1)
       *  ```
       */
      case t :: (_: New) :: (sel: Select) :: _
          if t.symbol == NoSymbol && sel.symbol.isConstructor =>
        Some(symbolAlternatives(sel.symbol.owner), namePos(t))

      /** Workaround for missing symbol in:
       *  ```
       *  class A[T](a: T)
       *  val x = <<A>>[Int](1)
       *  ```
       */
      case (sel @ Select(New(t), _)) :: (_: TypeApply) :: _
          if sel.symbol.isConstructor =>
        Some(symbolAlternatives(sel.symbol.owner), namePos(t))
      /* simple identifier:
       *  ```
       *  val a = val@@ue + value
       *  ```
       */
      case (id: Ident) :: _ =>
        Some(symbolAlternatives(id.symbol), id.sourcePos)
      /* simple selector:
       *  ```
       *  object.val@@ue
       *  ```
       */
      case (sel: Select) :: _ if selectNameSpan(sel).contains(pos.span) =>
        Some(symbolAlternatives(sel.symbol), pos.withSpan(sel.nameSpan))
      /* named argument:
       *  ```
       *  foo(nam@@e = "123")
       *  ```
       */
      case (arg: NamedArg) :: (appl: Apply) :: _ =>
        val realName = arg.name.stripModuleClassSuffix.lastPart
        if pos.span.start > arg.span.start && pos.span.end < arg.span.point + realName.length
        then
          val length = realName.toString.backticked.length()
          val pos = arg.sourcePos.withSpan(
            arg.span
              .withEnd(arg.span.start + length)
              .withPoint(arg.span.start)
          )
          appl.symbol.paramSymss.flatten.find(_.name == arg.name).map { s =>
            // if it's a case class we need to look for parameters also
            if caseClassSynthetics(s.owner.name) && s.owner.is(Flags.Synthetic)
            then
              (
                Set(
                  s,
                  s.owner.owner.companion.info.member(s.name).symbol,
                  s.owner.owner.info.member(s.name).symbol
                )
                  .filter(_ != NoSymbol),
                pos
              )
            else (Set(s), pos)
          }
        else None
      /* all definitions:
       *  ```
       *  def fo@@o = ???
       *  class Fo@@o = ???
       *  ```
       * etc.
       */
      case (df: NamedDefTree) :: _
          if df.nameSpan.contains(pos.span) && !isGeneratedGiven(df, sourceText) =>
        Some(symbolAlternatives(df.symbol), pos.withSpan(df.nameSpan))
      /* enum cases with params
       *  ```
       *  enum Foo:
       *   case B@@ar[A](i: A)
       *  ```
       */
      case (df: NamedDefTree) :: Template(_, _, self, _) :: _
          if (df.name == nme.apply || df.name == nme.unapply) && df.nameSpan.isZeroExtent =>
        Some(symbolAlternatives(self.tpt.symbol), self.sourcePos)

      /** For traversing annotations:
       *
       *  ```
       *  @JsonNo@@tification("")
       *  def params() = ???
       *  ```
       */
      case (df: MemberDef) :: _ if df.span.contains(pos.span) =>
        val annotTree = df.mods.annotations.find { t =>
          t.span.contains(pos.span)
        }
        collectTrees(annotTree).flatMap { t =>
          soughtSymbols(
            Interactive.pathTo(t, pos.span)
          )
        }.headOption

      /* Import selectors:
       * `import scala.util.Tr@@y`
       */
      case (imp: ImportOrExport) :: _ if imp.span.contains(pos.span) =>
        imp
          .selector(pos.span)
          .map(sym => (symbolAlternatives(sym), sym.sourcePos))

      /* Named tuple field access:
       *  ```
       *  val x: (name: String) = ???
       *  x.na@@me
       *  ```
       */
      case (app @ NamedTupleSelection(_, names, values, i)) :: _
          if namedTupleFieldSpan(app).contains(pos.span) =>
        val fieldSpan = namedTupleFieldSpan(app)
        namedTupleFieldSymbol(names, values, i, fieldSpan).map: sym =>
          (Set(sym), pos.withSpan(fieldSpan))

      case _ => None

    sought match
      case None => seekInExtensionParameters()
      case _ => sought

  end soughtSymbols

  private def seekInExtensionParameters() =
    def collectParams(
        extMethods: ExtMethods
    ): Option[ExtensionParamOccurrence] =
      NavigateAST
        .pathTo(pos.span, extMethods.paramss.flatten)(using compilatonUnitContext)
        .collectFirst {
          case v: untpd.ValOrTypeDef =>
            ExtensionParamOccurrence(
              v.name,
              v.namePos,
              v.symbol,
              extMethods.methods
            )
          case i: untpd.Ident =>
            ExtensionParamOccurrence(
              i.name,
              i.sourcePos,
              i.symbol,
              extMethods.methods
            )
        }

    for
      extensionMethodScope <- extensionMethods
      occurrence           <- collectParams(extensionMethodScope)
      symbols              <- collectAllExtensionParamSymbols(
        path.headOption.getOrElse(unit.tpdTree),
        occurrence
      )
    yield symbols

  private def collectAllExtensionParamSymbols(
      tree: tpd.Tree,
      occurrence: ExtensionParamOccurrence
  ): Option[(Set[Symbol], SourcePosition)] =
    occurrence match
      case ExtensionParamOccurrence(_, namePos, symbol, _)
          if symbol != NoSymbol && !symbol.isError && !symbol.owner.is(
            Flags.ExtensionMethod
          ) =>
        Some((symbolAlternatives(symbol), namePos))
      case ExtensionParamOccurrence(name, namePos, _, methods) =>
        val symbols =
          for
            method <- methods.toSet
            symbol <-
              Interactive.pathTo(tree, method.span) match
                case (d: DefDef) :: _ =>
                  d.paramss.flatten.collect {
                    case param if param.name.decoded == name.decoded =>
                      param.symbol
                  }
                case _ => Set.empty[Symbol]
            if (symbol != NoSymbol && !symbol.isError)
            withAlt <- symbolAlternatives(symbol)
          yield withAlt
        if symbols.nonEmpty then Some((symbols, namePos)) else None
  end collectAllExtensionParamSymbols

  private def findAllExtensionParamSymbols(
      pos: SourcePosition,
      name: Name,
      sym: Symbol
  ) =
    val symbols =
      for
        methods <- extensionMethods.map(_.methods)
        symbols <- collectAllExtensionParamSymbols(
          unit.tpdTree,
          ExtensionParamOccurrence(name, pos, sym, methods)
        )
      yield symbols
    symbols.getOrElse((symbolAlternatives(sym), pos))
end PcSymbolSearch

object PcSymbolSearch:
  // NOTE: Connected to https://github.com/lampepfl/dotty/issues/16771
  // `sel.nameSpan` is calculated incorrectly in (1 + 2).toString
  // See test DocumentHighlightSuite.select-parentheses
  def selectNameSpan(sel: Select): Span =
    val span = sel.span
    if span.exists then
      val point = span.point
      if sel.name.toTermName == nme.ERROR then Span(point)
      else if sel.qualifier.span.start > span.point then // right associative
        val realName = sel.name.stripModuleClassSuffix.lastPart
        Span(span.start, span.start + realName.length, point)
      else Span(point, span.end, point)
    else span

  def collectTrees(trees: Iterable[Positioned]): Iterable[Tree] =
    trees.collect { case t: Tree => t }

  def namePos(tree: Tree)(using Context): SourcePosition =
    tree match
      case sel: Select => sel.sourcePos.withSpan(selectNameSpan(sel))
      case _ => tree.sourcePos

  def isGeneratedGiven(df: NamedDefTree, sourceText: String)(using Context) =
    val nameSpan = df.nameSpan
    df.symbol.is(Flags.Given) && sourceText.substring(
      nameSpan.start,
      nameSpan.end
    ) != df.name.toString()

  def isNamedTupleApply(sym: Symbol)(using Context): Boolean =
    sym.exists && sym.name == nme.apply && sym.owner == defn.NamedTupleModule.moduleClass

  /** A named tuple field selection `qual.name` is typed as
   *  `NamedTuple.apply[Names, Values](qual)(idx)` with a synthetic (zero-extent)
   *  index literal, unlike an explicit `qual.apply(idx)`. Extracts
   *  `(qual, Names, Values, idx)`.
   */
  object NamedTupleSelection:
    def unapply(tree: Tree)(using Context): Option[(Tree, Tree, Tree, Int)] = tree match
      case Apply(
            Apply(TypeApply(fun, List(names, values)), List(qual)),
            List(index @ Literal(Constant(i: Int)))
          ) if isNamedTupleApply(fun.symbol) && (!index.span.exists || index.span.isZeroExtent) =>
        Some((qual, names, values, i))
      case _ => None

  /** The span of the selected field name in a named tuple selection. */
  def namedTupleFieldSpan(app: Tree): Span =
    app.span.withStart(app.span.point)

  /** The name and type of the `i`-th field of a named tuple with names `names`
   *  and values `values`. The tuple types may be `TupleN`, `*:` chains (e.g.
   *  `NamedTuple.From[Case]` or more than 22 fields) or type aliases.
   */
  def namedTupleFieldAt(names: Tree, values: Tree, i: Int)(using Context): Option[(TermName, Type)] =
    for
      nameTypes <- names.tpe.tupleElementTypes
      case ConstantType(Constant(name: String)) <- nameTypes.get(i).map(_.dealias)
    yield
      val fieldType = values.tpe.tupleElementTypes.flatMap(_.get(i)).getOrElse(NoType)
      (termName(name), fieldType)

  /** Named tuple fields have no symbol, so we synthesize one for each selection.
   *  Like a case class field, a named tuple field belongs to the tuple type:
   *  its identity is the field name together with the names and values of the
   *  tuple, which is stored as the symbol's info (see [[sameNamedTupleField]]).
   */
  def namedTupleFieldSymbol(names: Tree, values: Tree, i: Int, span: Span)(using Context): Option[Symbol] =
    namedTupleFieldAt(names, values, i).map: (name, _) =>
      newSymbol(
        NoSymbol,
        name,
        Flags.Synthetic,
        defn.NamedTuple(names.tpe, values.tpe),
        coord = spanCoord(span)
      )

  /** Whether two symbols from [[namedTupleFieldSymbol]] denote the same field,
   *  i.e. the same name on the same named tuple type.
   */
  def sameNamedTupleField(sym1: Symbol, sym2: Symbol)(using Context): Boolean =
    sym1.name == sym2.name
      && sym1.is(Flags.Synthetic) && sym2.is(Flags.Synthetic)
      && sym1.info =:= sym2.info

end PcSymbolSearch
