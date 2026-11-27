package dotty.tools.pc

import dotty.tools.dotc.ast.NavigateAST
import dotty.tools.dotc.ast.desugar
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Names.{termName, TermName}
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.TypeError
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.util.Spans.{spanCoord, Span}

/** Named tuple fields have no symbols. A field is declared by a name literal of its tuple type
 *  `(name: String)`, constructed by `(name = "a")`, selected by `x.name`, which the typer expands
 *  to `NamedTuple.apply[Names, Values](x)(idx)`, and matched by the label of a pattern `(name = n)`,
 *  which the typer drops. So that the tools can treat these occurrences like those of a member,
 *  a symbol is synthesized for each of them, see [[NamedTupleField]].
 */
object NamedTupleFields:

  /** An occurrence of a named tuple field with its synthesized symbol. The symbol is owned by
   *  no symbol, flagged `Synthetic`, positioned at the occurrence, and, like a case class field
   *  belongs to its class, has the tuple type as its info. `span` is the span of the name at the
   *  occurrence, without backticks or quotes.
   */
  case class NamedTupleField(symbol: Symbol, span: Span)

  private def field(tupleType: Type, name: TermName, span: Span)(using Context): NamedTupleField =
    NamedTupleField(newSymbol(NoSymbol, name, Flags.Synthetic, tupleType, coord = spanCoord(span)), span)

  /** Is `sym` synthesized for a named tuple field? */
  def isNamedTupleField(sym: Symbol)(using Context): Boolean =
    sym.isTerm && sym.is(Flags.Synthetic) && sym.owner == NoSymbol

  /** Fields are the same when they have the same name in named tuples with the same field names.
   *  The field types are not compared: a constructor `(xs = Nil)` or a generic `(x = t)` has
   *  a subtype of the declared `(xs: List[Int])`, and a rename has to change all of them.
   */
  def sameNamedTupleField(sym: Symbol, field: NamedTupleField)(using Context): Boolean =
    isNamedTupleField(sym) && sym.name == field.symbol.name
      && namedTupleFieldNames(sym.info) == namedTupleFieldNames(field.symbol.info)

  def isNamedTupleMethod(sym: Symbol, name: TermName)(using Context): Boolean =
    sym.exists && sym.name == name && sym.owner == defn.NamedTupleModule.moduleClass

  /** `qual.name` on a named tuple is typed as `NamedTuple.apply[Names, Values](qual)(idx)`, where
   *  the index literal gets a zero-extent span, unlike the index of an explicit `qual.apply(idx)`.
   */
  object NamedTupleSelection:
    def unapply(tree: Tree)(using Context): Option[(Tree, Tree, Tree, Int)] = tree match
      case Apply(Apply(TypeApply(fun, List(names, values)), List(qual)), List(index @ Literal(Constant(i: Int))))
          if isNamedTupleMethod(fun.symbol, nme.apply) && (!index.span.exists || index.span.isZeroExtent) =>
        Some((qual, names, values, i))
      case _ => None

  /** The `(names, values)` of a named tuple type `(n: T, ...)` or constructor `(n = t, ...)`. */
  object NamedTupleTree:
    def unapply(tree: Tree)(using Context): Option[(Tree, Tree)] = tree match
      case AppliedTypeTree(tycon, List(names, values)) if tycon.symbol == defn.NamedTupleTypeRef.symbol =>
        Some((names, values))
      case Apply(TypeApply(Apply(TypeApply(fun, List(names)), Nil), _), List(values))
          if isNamedTupleMethod(fun.symbol, nme.build) =>
        Some((names, values))
      case _ => None

  /** The fields of a named tuple type, `Nil` for any other type. */
  def namedTupleFieldsOf(tpe: Type)(using Context): List[(TermName, Type)] =
    try tpe.widenDealias.namedTupleElementTypes(derived = true)
    catch case _: TypeError => Nil // a name that is not a string literal

  /** The field names of a named tuple type, `Nil` for any other type. */
  def namedTupleFieldNames(tpe: Type)(using Context): List[TermName] =
    namedTupleFieldsOf(tpe).map(_._1)

  /** The `i`-th field of a named tuple type or constructor tree. */
  def namedTupleFieldAt(names: Tree, values: Tree, i: Int)(using Context): Option[(TermName, Type)] =
    namedTupleFieldsOf(defn.NamedTuple(names.tpe, values.tpe)).lift(i)

  /** `span` without the backticks or quotes around it in the source, like a definition's `nameSpan`. */
  private def unquoted(span: Span, sourceText: String): Span =
    sourceText.lift(span.start) match
      case Some('`') => span.withStart(span.start + 1).withEnd(span.end - 1)
      case Some('"') =>
        val quotes = if sourceText.startsWith("\"\"\"", span.start) then 3 else 1
        span.withStart(span.start + quotes).withEnd(span.end - quotes)
      case _ => span

  /** The field selected by `qual.name`, positioned at the name. */
  def selectedNamedTupleField(app: Tree, sourceText: String)(using Context): Option[NamedTupleField] = app match
    case NamedTupleSelection(_, names, values, i) =>
      val tupleType = defn.NamedTuple(names.tpe, values.tpe)
      namedTupleFieldsOf(tupleType).lift(i).map: (name, _) =>
        field(tupleType, name, unquoted(app.span.withStart(app.span.point), sourceText))
    case _ => None

  /** The name literals of a named tuple type tree with their names, in order;
   *  the tuple may be a `TupleN` or a `*:` chain.
   */
  private def nameLiterals(names: Tree): List[(Tree, TermName)] = names match
    case AppliedTypeTree(_, args) => args.flatMap(nameLiterals)
    case literal @ SingletonTypeTree(Literal(Constant(name: String))) => List((literal, termName(name)))
    case _ => Nil

  /** The fields of a named tuple type or constructor tree whose names are spelled out as literals,
   *  with those literals, positioned at the literals.
   *  The names of an explicit type `NamedTuple[Names, (String)]` are not spelled out.
   */
  def declaredNamedTupleFields(names: Tree, values: Tree, sourceText: String)(using
      Context): List[(Tree, NamedTupleField)] =
    val tupleType = defn.NamedTuple(names.tpe, values.tpe)
    val fieldNames = namedTupleFieldNames(tupleType)
    nameLiterals(names).collect:
      case (literal, name) if fieldNames.contains(name) =>
        (literal, field(tupleType, name, unquoted(literal.span, sourceText)))

  /** Are all the names of a named tuple type or constructor tree spelled out as literals? */
  def declaresAllNamedTupleFields(names: Tree, values: Tree)(using Context): Boolean =
    nameLiterals(names).map(_._2) == namedTupleFieldNames(defn.NamedTuple(names.tpe, values.tpe))

  /** The labels of a named tuple pattern `(name = n, ...)` with the tuple type they belong to;
   *  a nested pattern `(a = (b = n))` has the labels of the type of the field `a`.
   */
  private def patternLabels(pat: untpd.Tree, tupleType: Type)(using Context): List[(untpd.NamedArg, Type)] =
    pat match
      case untpd.Tuple(elems) =>
        val fields = namedTupleFieldsOf(tupleType)
        elems.zipWithIndex.flatMap:
          case (arg @ untpd.NamedArg(name, sub), _) =>
            fields.find(_._1 == name).toList.flatMap: (_, fieldType) =>
              (arg, tupleType) :: patternLabels(sub, fieldType)
          case (sub, i) => fields.lift(i).toList.flatMap((_, fieldType) => patternLabels(sub, fieldType))
      case untpd.Parens(pat) => patternLabels(pat, tupleType)
      case untpd.Bind(_, pat) => patternLabels(pat, tupleType)
      case untpd.Typed(pat, _) => patternLabels(pat, tupleType)
      case untpd.Alternative(pats) => pats.flatMap(patternLabels(_, tupleType))
      case _ => Nil

  /** The fields labelled in the named tuple patterns of a case, `case (name = n) =>`, positioned at the labels.
   *  The typer drops the labels, so they are found in the untyped tree.
   */
  def namedTuplePatternFields(cdef: CaseDef, scrutinee: Tree, untpdTree: untpd.Tree)(using
      Context): List[NamedTupleField] =
    val patSpan = cdef.pat.span
    if !patSpan.exists || patSpan.isZeroExtent || namedTupleFieldsOf(scrutinee.tpe).isEmpty then Nil
    else
      NavigateAST.pathTo(patSpan, List(untpdTree)).headOption.toList.flatMap:
        case pat: untpd.Tree =>
          patternLabels(pat, scrutinee.tpe).map: (arg, tupleType) =>
            field(tupleType, arg.name.asTermName, desugar.namedArgNameSpan(arg))
        case _ => Nil

end NamedTupleFields
