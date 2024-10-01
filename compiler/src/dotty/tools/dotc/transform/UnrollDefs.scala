package dotty.tools.dotc.transform

import dotty.tools.dotc.*
import core.*
import MegaPhase.MiniPhase
import Contexts.*
import Symbols.*
import Flags.*
import SymDenotations.*
import Decorators.*
import ast.Trees.*
import ast.tpd
import StdNames.nme
import Names.*
import Constants.Constant
import dotty.tools.dotc.core.NameKinds.DefaultGetterName
import dotty.tools.dotc.core.Types.{MethodType, NamedType, PolyType, Type}
import dotty.tools.dotc.core.Symbols
import dotty.tools.dotc.printing.Formatting.hl

import dotty.tools.dotc.core.Types.NoType
import dotty.tools.dotc.typer.Checking

class UnrollDefs extends MiniPhase {
  import tpd._

  val phaseName = "unroll"

  override val runsAfter = Set(FirstTransform.name)

  def copyParamSym(sym: Symbol, parent: Symbol)(using Context): (Symbol, Symbol) =
    sym -> sym.copy(owner = parent)

  def findUnrollAnnotations(params: List[Symbol])(using Context): List[Int] = {
    params
      .zipWithIndex
      .collect {
        case (v, i) if v.annotations.exists(_.symbol.fullName.toString == "scala.annotation.unroll") =>
          i
      }
  }
  def isTypeClause(p: ParamClause) = p.headOption.exists(_.isInstanceOf[TypeDef])

  def generateSingleForwarder(defdef: DefDef,
                              prevMethodType: Type,
                              paramIndex: Int,
                              nextParamIndex: Int,
                              nextSymbol: Symbol,
                              annotatedParamListIndex: Int,
                              isCaseApply: Boolean)(using Context) = {

    def extractParamSymss(parent: Symbol)(using Context): List[List[(Symbol, Symbol)]] =
      defdef.paramss.zipWithIndex.map{ case (ps, i) =>
        if (i == annotatedParamListIndex) ps.take(paramIndex).map(p => copyParamSym(p.symbol, parent))
        else ps.map(p => copyParamSym(p.symbol, parent))
      }

    val forwarderDefSymbol = Symbols.newSymbol(
      defdef.symbol.owner,
      defdef.name,
      defdef.symbol.flags &~
      HasDefaultParams &~
      (if (nextParamIndex == -1) Flags.EmptyFlags else Deferred) |
      Invisible,
      NoType, // fill in later
    )

    val newParamSymMappings = extractParamSymss(forwarderDefSymbol)
    val (oldParams, newParams) = newParamSymMappings.flatten.unzip

    val newParamSymLists =
      newParamSymMappings.map: pairss =>
        pairss.map: (oldSym, newSym) =>
          newSym.info = oldSym.info.substSym(oldParams, newParams)
          newSym

    val newResType = defdef.tpt.tpe.substSym(oldParams, newParams)
    forwarderDefSymbol.info = NamerOps.methodType(newParamSymLists, newResType)
    forwarderDefSymbol.setParamss(newParamSymLists)

    def forwarderRhs(): tpd.Tree = {
      val defaultOffset = defdef.paramss
        .iterator
        .take(annotatedParamListIndex)
        .filter(!isTypeClause(_))
        .map(_.size)
        .sum

      val defaultCalls = Range(paramIndex, nextParamIndex).map(n =>
        val inner = if (defdef.symbol.isConstructor) {
          ref(defdef.symbol.owner.companionModule)
            .select(DefaultGetterName(defdef.name, n + defaultOffset))
        } else if (isCaseApply) {
          ref(defdef.symbol.owner.companionModule)
            .select(DefaultGetterName(termName("<init>"), n + defaultOffset))
        } else {
          This(defdef.symbol.owner.asClass)
            .select(DefaultGetterName(defdef.name, n + defaultOffset))
        }

        newParamSymLists
          .take(annotatedParamListIndex)
          .map(_.map(p => ref(p)))
          .foldLeft[Tree](inner){
            case (lhs: Tree, newParams) =>
              if (newParams.headOption.exists(_.isInstanceOf[TypeTree])) TypeApply(lhs, newParams)
              else Apply(lhs, newParams)
          }
      )

      val forwarderInner: Tree = This(defdef.symbol.owner.asClass).select(nextSymbol)

      val forwarderCallArgs =
        newParamSymLists.zipWithIndex.map{case (ps, i) =>
          if (i == annotatedParamListIndex) ps.map(p => ref(p)).take(nextParamIndex) ++ defaultCalls
          else ps.map(p => ref(p))
        }

      val forwarderCall0 = forwarderCallArgs.foldLeft[Tree](forwarderInner){
        case (lhs: Tree, newParams) =>
          if (newParams.headOption.exists(_.isInstanceOf[TypeTree])) TypeApply(lhs, newParams)
          else Apply(lhs, newParams)
      }

      val forwarderCall =
        if (!defdef.symbol.isConstructor) forwarderCall0
        else Block(List(forwarderCall0), Literal(Constant(())))

      forwarderCall
    }

    val forwarderDef =
      tpd.DefDef(forwarderDefSymbol,
        rhs = if (nextParamIndex == -1) EmptyTree else forwarderRhs())

    forwarderDef
  }

  def generateFromProduct(startParamIndices: List[Int], paramCount: Int, defdef: DefDef)(using Context) = {
    cpy.DefDef(defdef)(
      name = defdef.name,
      paramss = defdef.paramss,
      tpt = defdef.tpt,
      rhs = Match(
        ref(defdef.paramss.head.head.asInstanceOf[ValDef].symbol).select(termName("productArity")),
        startParamIndices.map { paramIndex =>
          val Apply(select, args) = defdef.rhs: @unchecked
          CaseDef(
            Literal(Constant(paramIndex)),
            EmptyTree,
            Apply(
              select,
              args.take(paramIndex) ++
                Range(paramIndex, paramCount).map(n =>
                  ref(defdef.symbol.owner.companionModule)
                    .select(DefaultGetterName(defdef.symbol.owner.primaryConstructor.name.toTermName, n))
                )
            )
          )
        } ++ Seq(
          CaseDef(
            EmptyTree,
            EmptyTree,
            defdef.rhs
          )
        )
      )
    ).setDefTree
  }

  def generateSyntheticDefs(tree: Tree)(using Context): (Option[(Symbol, Seq[Symbol])], Seq[(Symbol, Tree)]) = tree match{
    case defdef: DefDef if defdef.paramss.nonEmpty =>
      import dotty.tools.dotc.core.NameOps.isConstructorName

      val isCaseCopy =
        defdef.name.toString == "copy" && defdef.symbol.owner.is(CaseClass)

      val isCaseApply =
        defdef.name.toString == "apply" && defdef.symbol.owner.companionClass.is(CaseClass)

      val isCaseFromProduct = defdef.name.toString == "fromProduct" && defdef.symbol.owner.companionClass.is(CaseClass)

      val annotated =
        if (isCaseCopy) defdef.symbol.owner.primaryConstructor
        else if (isCaseApply) defdef.symbol.owner.companionClass.primaryConstructor
        else if (isCaseFromProduct) defdef.symbol.owner.companionClass.primaryConstructor
        else defdef.symbol


      annotated
        .paramSymss
        .zipWithIndex
        .flatMap{case (paramClause, paramClauseIndex) =>
          val annotationIndices = findUnrollAnnotations(paramClause)
          if (annotationIndices.isEmpty) None
          else Some((paramClauseIndex, annotationIndices))
        }  match{
        case Nil => (None, Nil)
        case Seq((paramClauseIndex, annotationIndices)) =>
          val paramCount = annotated.paramSymss(paramClauseIndex).size
          if (isCaseFromProduct) {
            val newDef = generateFromProduct(annotationIndices, paramCount, defdef)
            (Some(defdef.symbol, Seq(newDef.symbol)), Seq(defdef.symbol -> newDef))
          } else {
            if (defdef.symbol.is(Deferred)){
              val replacements = Seq.newBuilder[Symbol]
              val newDefs =
                (-1 +: annotationIndices :+ paramCount).sliding(2).toList.foldLeft((Seq.empty[(Symbol, DefDef)], defdef.symbol))((m, v) => ((m, v): @unchecked) match {
                  case ((defdefs, nextSymbol), Seq(paramIndex, nextParamIndex)) =>
                    val forwarder = generateSingleForwarder(
                      defdef,
                      defdef.symbol.info,
                      nextParamIndex,
                      paramIndex,
                      nextSymbol,
                      paramClauseIndex,
                      isCaseApply
                    )
                    replacements += forwarder.symbol
                    ((defdef.symbol -> forwarder) +: defdefs, forwarder.symbol)
                })._1
              (
                Some(defdef.symbol, replacements.result()),
                newDefs
              )

            }else{

              (
                None,
                (annotationIndices :+ paramCount).sliding(2).toList.reverse.foldLeft((Seq.empty[(Symbol, DefDef)], defdef.symbol))((m, v) => ((m, v): @unchecked) match {
                  case ((defdefs, nextSymbol), Seq(paramIndex, nextParamIndex)) =>
                    val forwarder = generateSingleForwarder(
                      defdef,
                      defdef.symbol.info,
                      paramIndex,
                      nextParamIndex,
                      nextSymbol,
                      paramClauseIndex,
                      isCaseApply
                    )
                    ((defdef.symbol -> forwarder) +: defdefs, forwarder.symbol)
                })._1
              )
            }
          }

        case multiple => sys.error("Cannot have multiple parameter lists containing `@unroll` annotation")
      }

    case _ => (None, Nil)
  }

  override def transformTemplate(tmpl: tpd.Template)(using Context): tpd.Tree = {

    val (removed0, generatedDefs0) = tmpl.body.map(generateSyntheticDefs).unzip
    val (removedCtor, generatedConstr0) = generateSyntheticDefs(tmpl.constr)
    val removedFlat = removed0.flatten
    val removedSymsBody = removedFlat.map(_(0))
    val allRemoved = removedFlat ++ removedCtor

    val generatedDefOrigins = generatedDefs0.flatten
    val generatedDefs = generatedDefOrigins.map(_(1))
    val generatedConstr = generatedConstr0.map(_(1))

    val otherDecls = tmpl.body.filter(t => !removedSymsBody.contains(t.symbol))

    for (sym, replacements) <- allRemoved do
      val cls = sym.owner.asClass
      def totalParamCount(sym: Symbol): Int = sym.paramSymss.view.map(_.size).sum
      val symParamCount = totalParamCount(sym)
      val replaced = replacements.find(totalParamCount(_) == symParamCount).get
      cls.replace(sym, replaced)

    /** inlined from compiler/src/dotty/tools/dotc/typer/Checking.scala */
    def checkClash(decl: Symbol, other: Symbol) =
      def staticNonStaticPair = decl.isScalaStatic != other.isScalaStatic
      decl.matches(other) && !staticNonStaticPair

    if generatedDefOrigins.nonEmpty then
      val byName = otherDecls.groupMap(_.symbol.name.toString)(_.symbol)
      for case (src, dcl: NamedDefTree) <- generatedDefOrigins do
        val replaced = dcl.symbol
        def symLocation(sym: Symbol) = {
          val lineDesc =
            if (sym.span.exists && sym.span != sym.owner.span)
              s" at line ${sym.srcPos.line + 1}"
            else ""
          i"in ${sym.owner}${lineDesc}"
        }
        byName.get(dcl.name.toString).foreach { syms =>
          val clashes = syms.filter(checkClash(replaced, _))
          for existing <- clashes do
            report.error(i"""Unrolled $replaced clashes with existing declaration.
              |Please remove the clashing definition, or the @unroll annotation.
              |Unrolled from ${hl(src.showDcl)} ${symLocation(src)}""".stripMargin, existing.srcPos)
        }
    end if

    super.transformTemplate(
      cpy.Template(tmpl)(
        tmpl.constr,
        tmpl.parents,
        tmpl.derived,
        tmpl.self,
        otherDecls ++ generatedDefs ++ generatedConstr
      )
    )
  }
}
