package dotty.tools.pc.completions

import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Names.Name
import dotty.tools.dotc.core.StdNames.*
import dotty.tools.dotc.core.SymDenotations.NoDenotation
import dotty.tools.dotc.core.Symbols
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.util.SourcePosition
import dotty.tools.dotc.ast.NavigateAST

import scala.meta.internal.pc.CompletionFuzzy

object NamedPatternCompletions:

  def isInsideParams(sourcePos: SourcePosition, start: Int): Boolean =
    sourcePos.source.content().slice(sourcePos.start, start).foldLeft(0): (count, char) =>
      if char == '(' then count + 1
      else if char == ')' then count - 1
      else count
    > 0

  def unapply(path: List[Tree])(using Context): Option[CompletionPos => List[CompletionValue]] =
    val result = path match
      // case (nam@@
      // but not case nam@@
      case (bind: Bind) :: (caseDef: CaseDef) :: Match(selector, _) :: _
          if isInsideParams(caseDef.sourcePos, bind.sourcePos.end) =>
        if selector.tpe.widenDealias.isNamedTupleType then
          Some(selector.tpe.widenDealias.namedTupleElementTypes.toMap, Nil)
        else None

      // case (name = supername, na@@
      // case (nam@@, surname = test) =>
      case (_: Bind) :: (rest @ (unapply: UnApply) :: _)
        if defn.isTupleClass(unapply.fun.symbol.owner.companionClass) =>
          rest.collectFirst: // We can't complete names without knowing the type of selector
            case Match(selector, _) => selector
          .flatMap: selector =>
            // Named patterns are desugared to normal binds without original arg name info
            val patterns = NavigateAST.untypedPath(unapply).collectFirst:
              case untpd.Tuple(elems) => elems
            .getOrElse(Nil)

            if selector.tpe.widenDealias.isNamedTupleType then
              Some(selector.tpe.widenDealias.namedTupleElementTypes.toMap, patterns)
            else None

      // case User(nam@@
      // case User(nam@@, surname = test) =>
      case (_: Bind) :: (rest @ (unapply: UnApply) :: _) =>
        Some(unapplyResultNamesToTypes(unapply.fun), unapply.patterns)

      // This case is happening because nam@@ is removed at desugaring as it is illegal unnamed bind mixed with named one
      // case User(surname = test, nam@@) =>
      // case User(surname = test, nam@@
      case UnApply(fun, _, patterns) :: _ => Some(unapplyResultNamesToTypes(fun), patterns)
      case _ => None

    result.map: (namesToArgs, patterns) =>
      contribute(_, namesToArgs, patterns)
  end unapply

  private object NamedTupleUnapplyResultType:
    def unapply(tree: Type)(using Context): Option[Type] = tree match
      case AppliedType(TypeRef(_, cls), (namedTuple @ defn.NamedTuple(_, _)) :: Nil)
          if (cls == ctx.definitions.OptionClass || cls == ctx.definitions.SomeClass) => Some(namedTuple)
      case _ => None

  private def unapplyResultNamesToTypes(tree: Tree)(using Context): Map[Name, Type] =
    tree.tpe.widenDealias.finalResultType match
      // result type is named tuple, we can directly extract names
      case AppliedType(TypeRef(_, cls), (namedTuple @ defn.NamedTuple(_, _)) :: Nil)
          if (cls == ctx.definitions.OptionClass || cls == ctx.definitions.SomeClass) =>
            namedTuple.namedTupleElementTypes.toMap
      // unapplies generated for case classes have synthetic names and result type is not a named tuple
      case _ if tree.symbol.flags.is(Flags.Synthetic) =>
        val apply = tree.symbol.owner.info.member(nme.apply)
        val maybeApplied = tree match
          // The check for case flag is necessary to filter introduced type bounds T$1..n
          case tpeApply @ TypeApply(_, args) if !args.exists(_.symbol.flags.is(Flags.Case)) =>
            apply.info.appliedTo(args.map(_.tpe))
          case _ => apply.info

        val unapplyParamList = maybeApplied.paramNamess.indexWhere(_.forall(_.isTermName))
        if unapplyParamList < 0 then Map.empty
        else
          val paramNames= maybeApplied.paramNamess(unapplyParamList)
          val paramInfos = maybeApplied.paramInfoss(unapplyParamList)
          (paramNames zip paramInfos).toMap
      case _ => Map.empty // we can't help complete non synthetic non named tuple extractors

  def contribute(
    completionPos: CompletionPos,
    namesToArgs: Map[Name, Type],
    patterns: List[untpd.Tree]
  )(using Context): List[CompletionValue] =
    val usedNames = patterns.collect:
      case untpd.NamedArg(name, _) => name.asTermName

    val remainingParams = namesToArgs -- usedNames
    remainingParams
      .toList
      .filter: (name, _) =>
        CompletionFuzzy.matchesSubCharacters(completionPos.query, name.toString)
      .map: (name, tpe) =>
        CompletionValue.NamedArg(name.show + " = ", tpe, NoDenotation)

