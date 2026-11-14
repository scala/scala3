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
      .dropWhile(t => // NamedArg and named tuple field names don't have symbols anyway
        t.symbol == NoSymbol && !t.isInstanceOf[NamedArg] && !t.isInstanceOf[SingletonTypeTree] ||
          // same issue https://github.com/lampepfl/dotty/issues/15937 as below
          t.isInstanceOf[TypeTree]
      )

  lazy val extensionMethods =
    NavigateAST
      .untypedPath(pos.span)(using compilatonUnitContext)
      .collectFirst { case em @ ExtMethods(_, _) => em }

  lazy val path = rawPath match
    // For type it will sometimes go into the wrong tree since TypeTree also contains the same span
    // https://github.com/lampepfl/dotty/issues/15937
    case TypeApply(sel: Select, _) :: _ if sel.span.contains(pos.span) =>
      Interactive.pathTo(sel, pos.span) ::: rawPath
    case _ => skipNamedTupleApply(rawPath)

  // The `NamedTuple.apply` synthesized for `x.name` has the span of `x`, so go into the qualifier
  private def skipNamedTupleApply(path: List[Tree]): List[Tree] = path match
    case (sel: Select) :: (_: TypeApply) :: Apply(_, List(qual)) :: rest
        if isNamedTupleMethod(sel.symbol, nme.apply) && qual.span.contains(pos.span) =>
      skipNamedTupleApply(Interactive.pathTo(qual, pos.span)) ::: rest
    case _ => path

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

      /* named tuple field:
       *  ```
       *  x.na@@me
       *  ```
       */
      case (app @ NamedTupleSelection(_, names, values, i)) :: _
          if namedTupleFieldSpan(app, sourceText).contains(pos.span) =>
        val span = namedTupleFieldSpan(app, sourceText)
        namedTupleFieldAt(names, values, i).map: (name, _) =>
          (Set(namedTupleFieldSymbol(defn.NamedTuple(names.tpe, values.tpe), name, span)), pos.withSpan(span))
      /* named tuple field declaration:
       *  ```
       *  val x: (na@@me: String) = (na@@me = "")
       *  ```
       */
      case NamedTupleFieldName(name, names, values, i) =>
        val span = namedTupleFieldNameSpan(name, sourceText)
        namedTupleFieldAt(names, values, i).map: (fieldName, _) =>
          (Set(namedTupleFieldSymbol(defn.NamedTuple(names.tpe, values.tpe), fieldName, span)), pos.withSpan(span))
      /* named tuple pattern label:
       *  ```
       *  x match
       *    case (na@@me = n) => n
       *  ```
       */
      case _ =>
        path.zip(path.drop(1)).collectFirst:
          case (cdef: CaseDef, Match(scrutinee, _)) if cdef.pat.span.contains(pos.span) =>
            namedTuplePatternLabels(cdef, scrutinee, unit.untpdTree).collectFirst:
              case (span, name, tupleType) if span.contains(pos.span) =>
                (Set(namedTupleFieldSymbol(tupleType, name, span)), pos.withSpan(span))
        .flatten

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

  def isNamedTupleMethod(sym: Symbol, name: TermName)(using Context): Boolean =
    sym.exists && sym.name == name && sym.owner == defn.NamedTupleModule.moduleClass

  /** `qual.name` on a named tuple is typed as `NamedTuple.apply[Names, Values](qual)(idx)`
   *  with a synthetic index, unlike an explicit `qual.apply(idx)`.
   */
  object NamedTupleSelection:
    def unapply(tree: Tree)(using Context): Option[(Tree, Tree, Tree, Int)] = tree match
      case Apply(Apply(TypeApply(fun, List(names, values)), List(qual)), List(index @ Literal(Constant(i: Int))))
          if isNamedTupleMethod(fun.symbol, nme.apply) && (!index.span.exists || index.span.isZeroExtent) =>
        Some((qual, names, values, i))
      case _ => None

  /** The `NamedTuple.apply` reference of a selection, which has the span of the qualifier. */
  def namedTupleSelectionFun(app: Tree): Tree = app match
    case Apply(Apply(TypeApply(fun, _), _), _) => fun
    case _ => EmptyTree

  /** The `(names, values)` of a named tuple type `(n: T, ...)` or constructor `(n = t, ...)`. */
  object NamedTupleTree:
    def unapply(tree: Tree)(using Context): Option[(Tree, Tree)] = tree match
      case AppliedTypeTree(tycon, List(names, values)) if tycon.symbol == defn.NamedTupleTypeRef.symbol =>
        Some((names, values))
      case Apply(TypeApply(Apply(TypeApply(fun, List(names)), Nil), _), List(values))
          if isNamedTupleMethod(fun.symbol, nme.build) =>
        Some((names, values))
      case _ => None

  /** The name literals of a named tuple, in order; the tuple may be `TupleN` or a `*:` chain. */
  def namedTupleFieldNames(names: Tree): List[Tree] = names match
    case AppliedTypeTree(_, args) => args.flatMap(namedTupleFieldNames)
    case name @ SingletonTypeTree(Literal(Constant(_: String))) => List(name)
    case _ => Nil

  /** The span of a field name literal, without the quotes of an explicit literal type `NamedTuple[("name"), (String)]`. */
  def namedTupleFieldNameSpan(name: Tree, sourceText: String): Span =
    val span = name.span
    if sourceText.lift(span.start).contains('"') then
      val quotes = if sourceText.startsWith("\"\"\"", span.start) then 3 else 1
      span.withStart(span.start + quotes).withEnd(span.end - quotes)
    else span

  /** `(na@@me: String)`: the name literal under the cursor, which declares the field, with its tuple and index. */
  object NamedTupleFieldName:
    def unapply(path: List[Tree])(using Context): Option[(Tree, Tree, Tree, Int)] = path match
      case (name: SingletonTypeTree) :: rest =>
        for
          (names, values) <- rest.collectFirst { case NamedTupleTree(names, values) => (names, values) }
          i = namedTupleFieldNames(names).indexWhere(_ eq name)
          if i >= 0
        yield (name, names, values, i)
      case _ => None

  /** The span of the field name in `qual.name`, without backticks like a definition's `nameSpan`. */
  def namedTupleFieldSpan(app: Tree, sourceText: String): Span =
    val span = app.span.withStart(app.span.point)
    if sourceText.lift(span.start).contains('`') then span.withStart(span.start + 1).withEnd(span.end - 1) else span

  /** The fields of a named tuple type, `Nil` for any other type. */
  def namedTupleFieldsOf(tpe: Type)(using Context): List[(TermName, Type)] = tpe.widenDealias.normalized match
    case defn.NamedTupleDirect(names, values) =>
      val types = values.tupleElementTypes.getOrElse(Nil)
      names.tupleElementTypes.getOrElse(Nil).map(_.dealias).zipWithIndex.collect:
        case (ConstantType(Constant(name: String)), i) => (termName(name), types.lift(i).getOrElse(NoType))
    case _ => Nil

  /** The `i`-th field of a named tuple type or constructor tree. */
  def namedTupleFieldAt(names: Tree, values: Tree, i: Int)(using Context): Option[(TermName, Type)] =
    namedTupleFieldsOf(defn.NamedTuple(names.tpe, values.tpe)).lift(i)

  /** Named tuple fields have no symbol, so we synthesize one. Like a case class field it belongs
   *  to the tuple type, which is kept as the symbol's info. The span keeps `sourcePos` distinct.
   */
  def namedTupleFieldSymbol(tupleType: Type, name: TermName, span: Span)(using Context): Symbol =
    newSymbol(NoSymbol, name, Flags.Synthetic, tupleType, coord = spanCoord(span))

  def isNamedTupleField(sym: Symbol)(using Context): Boolean = sym.is(Flags.Synthetic) && sym.owner == NoSymbol

  /** Fields are the same when they have the same name in named tuples with the same field names.
   *  The field types are not compared: a constructor `(xs = Nil)` or a generic `(x = t)` has
   *  a subtype of the declared `(xs: List[Int])`, and a rename has to change all of them.
   */
  def sameNamedTupleField(sym: Symbol, name: TermName, tupleType: Type)(using Context): Boolean =
    isNamedTupleField(sym) && sym.name == name
      && namedTupleFieldsOf(sym.info).map(_._1) == namedTupleFieldsOf(tupleType).map(_._1)

  /** The field labels of a named tuple pattern `case (name = n, ...)`, which the typer drops,
   *  as `(span, name, tupleType)`; nested patterns `(a = (b = n))` have the labels of the field's type.
   */
  def namedTuplePatternLabels(pat: untpd.Tree, tupleType: Type)(using Context): List[(Span, TermName, Type)] =
    pat match
      case untpd.Tuple(elems) =>
        val fields = namedTupleFieldsOf(tupleType)
        elems.zipWithIndex.flatMap:
          case (arg @ untpd.NamedArg(name: TermName, sub), _) =>
            fields.find(_._1 == name).toList.flatMap: (_, fieldType) =>
              val point = arg.span.point
              (Span(point, point + name.toString.length, point), name, tupleType)
                :: namedTuplePatternLabels(sub, fieldType)
          case (sub, i) => fields.lift(i).toList.flatMap((_, fieldType) => namedTuplePatternLabels(sub, fieldType))
      case untpd.Parens(pat) => namedTuplePatternLabels(pat, tupleType)
      case untpd.Bind(_, pat) => namedTuplePatternLabels(pat, tupleType)
      case untpd.Typed(pat, _) => namedTuplePatternLabels(pat, tupleType)
      case untpd.Alternative(pats) => pats.flatMap(namedTuplePatternLabels(_, tupleType))
      case _ => Nil

  /** The labels of the named tuple patterns of a case, found in the untyped tree. */
  def namedTuplePatternLabels(cdef: CaseDef, scrutinee: Tree, untpdTree: untpd.Tree)(using
      Context): List[(Span, TermName, Type)] =
    if namedTupleFieldsOf(scrutinee.tpe).isEmpty then Nil
    else
      NavigateAST.pathTo(cdef.pat.span, List(untpdTree)).headOption.toList.flatMap:
        case pat: untpd.Tree => namedTuplePatternLabels(pat, scrutinee.tpe)
        case _ => Nil

end PcSymbolSearch
