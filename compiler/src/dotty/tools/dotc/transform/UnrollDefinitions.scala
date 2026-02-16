package dotty.tools.dotc
package transform

import ast.tpd
import ast.Trees.*
import core.*
import Flags.*
import Decorators.*
import Contexts.*
import Symbols.*
import Constants.Constant
import Decorators.*
import DenotTransformers.IdentityDenotTransformer
import Names.*
import dotty.tools.dotc.core.NameKinds.DefaultGetterName

import dotty.tools.dotc.core.Types.{NoPrefix, NoType}

import dotty.tools.dotc.printing.Formatting.hl

import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.unreachable

/**Implementation of SIP-61.
 * Runs when `@unroll` annotations are found in a compilation unit, installing new definitions
 *
 * Note that it only generates `Invisible` methods, so no interactions with Zinc/SemanticDB
 */
class UnrollDefinitions extends MacroTransform, IdentityDenotTransformer {
  self =>

  import tpd.*

  private val _unrolledDefs: util.EqHashMap[Symbol, ComputedIndices] = new util.EqHashMap[Symbol, ComputedIndices]
  private def initializeUnrolledDefs(): util.EqHashMap[Symbol, ComputedIndices] =
    _unrolledDefs.clear()
    _unrolledDefs

  override def phaseName: String = UnrollDefinitions.name

  override def description: String = UnrollDefinitions.description

  override def changesMembers: Boolean = true

  override def run(using Context): Unit =
    if ctx.compilationUnit.hasUnrollDefs then
      super.run // create and run the transformer on the current compilation unit

  def newTransformer(using Context): Transformer =
    UnrollingTransformer(ctx.compilationUnit.unrolledClasses)

  type ComputedIndices = List[(Int, List[Int])]
  type ComputeIndices = Context ?=> Symbol => ComputedIndices

  private class UnrollingTransformer(unrolledClasses: Set[Symbol]) extends Transformer {
    private val unrolledDefs = initializeUnrolledDefs()

    def computeIndices(annotated: Symbol)(using Context): ComputedIndices =
      unrolledDefs.getOrElseUpdate(annotated, {
        if annotated.name.is(DefaultGetterName) then
          // happens in curried methods, where default argument occurs in parameter list
          // after the unrolled parameter list.
          // example:
          //   `final def foo(@unroll y: String = "")(x: Int = 23) = x`
          // yields:
          //   `def foo$default$2(@unroll y: String): Int @uncheckedVariance = 23`
          // Perhaps annotations should be preprocessed before they are copied?
          Nil
        else
          val indices = annotated
            .paramSymss
            .zipWithIndex
            .flatMap: (paramClause, paramClauseIndex) =>
              val annotationIndices = findUnrollAnnotations(paramClause)
              if (annotationIndices.isEmpty) None
              else Some((paramClauseIndex, annotationIndices))
          if indices.nonEmpty then
            // pre-validation should have occurred in posttyper
            assert(!annotated.isLocal, i"$annotated is local")
            assert(annotated.isEffectivelyFinal || annotated.name.is(DefaultGetterName),
              i"$annotated is not final&concrete, or a constructor")
          indices
      })
    end computeIndices

    override def transform(tree: tpd.Tree)(using Context): tpd.Tree = tree match
      case tree @ TypeDef(_, impl: Template) if unrolledClasses(tree.symbol) =>
        super.transform(cpy.TypeDef(tree)(rhs = unrollTemplate(impl, computeIndices)))
      case tree =>
        super.transform(tree)
  }

  private def copyParamSym(sym: Symbol, parent: Symbol)(using Context): (Symbol, Symbol) =
    val copied = sym.copy(owner = parent, flags = (sym.flags &~ HasDefault), coord = sym.coord)
    sym -> copied

  private def symLocation(sym: Symbol)(using Context) = {
    val lineDesc =
      if (sym.span.exists && sym.span != sym.owner.span)
        s" at line ${sym.srcPos.line + 1}"
      else ""
    i"in ${sym.owner}${lineDesc}"
  }

  private def findUnrollAnnotations(params: List[Symbol])(using Context): List[Int] = {
    params
      .zipWithIndex
      .collect {
        case (v, i) if v.hasAnnotation(defn.UnrollAnnot) =>
          i
      }
  }

  private def isTypeClause(p: ParamClause) = p.headOption.exists(_.isInstanceOf[TypeDef])

  /** Generate a forwarder that calls the next one in a "chain" of forwarders
   *
   * @param defdef the original unrolled def that the forwarder is derived from
   * @param paramIndex index of the unrolled parameter (in the parameter list) that we stop at
   * @param paramCount number of parameters in the annotated parameter list
   * @param nextParamIndex index of next unrolled parameter - to fetch default argument
   * @param annotatedParamListIndex index of the parameter list that contains unrolled parameters
   * @param isCaseApply if `defdef` is a case class apply/constructor - used for selection of default arguments
   */
  private def generateSingleForwarder(defdef: DefDef,
                              paramIndex: Int,
                              paramCount: Int,
                              nextParamIndex: Int,
                              annotatedParamListIndex: Int,
                              isCaseApply: Boolean)(using Context): DefDef = {

    def initNewForwarder()(using Context): (TermSymbol, List[List[Symbol]]) = {
      val forwarderDefSymbol0 = Symbols.newSymbol(
        defdef.symbol.owner,
        defdef.name,
        defdef.symbol.flags &~ HasDefaultParams |
        Invisible | Synthetic,
        NoType, // fill in later
        coord = defdef.span
      ).entered

      val newParamSymMappings = extractParamSymss(copyParamSym(_, forwarderDefSymbol0))
      val (oldParams, newParams) = newParamSymMappings.flatten.unzip

      val newParamSymLists0 =
        newParamSymMappings.map: pairs =>
          pairs.map: (oldSym, newSym) =>
            newSym.info = oldSym.info.substSym(oldParams, newParams)
            newSym

      val newResType = defdef.tpt.tpe.substSym(oldParams, newParams)
      forwarderDefSymbol0.info = NamerOps.methodType(newParamSymLists0, newResType)
      forwarderDefSymbol0.setParamss(newParamSymLists0)
      forwarderDefSymbol0 -> newParamSymLists0
    }

    def extractParamSymss[T](onSymbol: Symbol => T): List[List[T]] =
      defdef.paramss.zipWithIndex.map{ case (ps, i) =>
        if (i == annotatedParamListIndex) ps.take(paramIndex).map(p => onSymbol(p.symbol))
        else ps.map(p => onSymbol(p.symbol))
      }

    val (forwarderDefSymbol, newParamSymLists) = initNewForwarder()

    def forwarderRhs(): tpd.Tree = {
      val defaultOffset = defdef.paramss
        .iterator
        .take(annotatedParamListIndex)
        .filter(!isTypeClause(_))
        .map(_.size)
        .sum

      val defaultCalls = Range(paramIndex, paramCount).map(n =>

        def makeSelect(refTree: Tree, name: TermName): Tree =
          val sym = refTree.symbol
          if !sym.findMember(name, NoPrefix, EmptyFlags, EmptyFlags).exists then
            val param = defdef.paramss(annotatedParamListIndex)(n)
            val methodStr = s"method ${defdef.name} ${symLocation(defdef.symbol)}"
            val paramStr = s"parameter ${param.name}"
            val errorMessage =
              i"Cannot unroll $methodStr because $paramStr needs a default value"
            report.error(errorMessage, param.srcPos)
            ref(newErrorSymbol(sym, nme.ERROR, errorMessage.toMessage))
          else
            refTree.select(name)

        val inner = if (defdef.symbol.isConstructor) {
          makeSelect(ref(defdef.symbol.owner.companionModule),
            DefaultGetterName(defdef.name, n + defaultOffset))
        } else if (isCaseApply) {
          makeSelect(ref(defdef.symbol.owner.companionModule),
            DefaultGetterName(termName("<init>"), n + defaultOffset))
        } else {
          makeSelect(This(defdef.symbol.owner.asClass),
            DefaultGetterName(defdef.name, n + defaultOffset))
        }

        newParamSymLists
          .take(annotatedParamListIndex)
          .map(_.map(ref))
          .foldLeft(inner)(_.appliedToArgs(_))
      )

      val forwarderInner: Tree =
        This(defdef.symbol.owner.asClass).select(defdef.symbol)

      val forwarderCallArgs =
        newParamSymLists.zipWithIndex.map{case (ps, i) =>
          if (i == annotatedParamListIndex) ps.map(ref).take(nextParamIndex) ++ defaultCalls
          else ps.map(ref)
        }

      val forwarderCall0 = forwarderCallArgs.foldLeft(forwarderInner)(_.appliedToArgs(_))

      val forwarderCall =
        if (!defdef.symbol.isConstructor) forwarderCall0
        else Block(List(forwarderCall0), Literal(Constant(())))

      forwarderCall
    }

    val forwarderDef =
      tpd.DefDef(forwarderDefSymbol, rhs = forwarderRhs()).withSpan(defdef.span)

    forwarderDef
  }

  case class Forwarders(origin: Symbol, forwarders: List[DefDef])

  private def generateSyntheticDefs(tree: Tree, compute: ComputeIndices)(using Context): Option[Forwarders] = tree match {
    case defdef: DefDef if defdef.paramss.nonEmpty =>
      import dotty.tools.dotc.core.NameOps.isConstructorName

      val isCaseCopy =
        defdef.name == nme.copy && defdef.symbol.owner.is(CaseClass)

      val isCaseApply =
        defdef.name == nme.apply && defdef.symbol.owner.companionClass.is(CaseClass)

      val annotated =
        if (isCaseCopy) defdef.symbol.owner.primaryConstructor
        else if (isCaseApply) defdef.symbol.owner.companionClass.primaryConstructor
        else defdef.symbol

      compute(annotated) match {
        case Nil => None
        case (paramClauseIndex, annotationIndices) :: Nil =>
          val paramCount = annotated.paramSymss(paramClauseIndex).size
          val generatedDefs =
            val indices = (annotationIndices :+ paramCount).sliding(2).toList.reverse
            indices.foldLeft(List.empty[DefDef]):
              case (defdefs, paramIndex :: nextParamIndex :: Nil) =>
                generateSingleForwarder(
                  defdef,
                  paramIndex,
                  paramCount,
                  nextParamIndex,
                  paramClauseIndex,
                  isCaseApply
                ) :: defdefs
              case _ => unreachable("sliding with at least 2 elements")
          Some(Forwarders(origin = defdef.symbol, forwarders = generatedDefs))

        case multiple =>
          report.error("Cannot have multiple parameter lists containing `@unroll` annotation", defdef.srcPos)
          None
      }

    case _ => None
  }

  private def unrollTemplate(tmpl: tpd.Template, compute: ComputeIndices)(using Context): tpd.Tree = {

    val generatedBody = tmpl.body.flatMap(generateSyntheticDefs(_, compute))
    val generatedConstr0 = generateSyntheticDefs(tmpl.constr, compute)
    val allGenerated = generatedBody ++ generatedConstr0

    if allGenerated.nonEmpty then
      val byName = (tmpl.constr :: tmpl.body).groupMap(_.symbol.name.toString)(_.symbol)
      for
        syntheticDefs <- allGenerated
        dcl <- syntheticDefs.forwarders
      do
        val replaced = dcl.symbol
        byName.get(dcl.name.toString).foreach { syms =>
          val clashes = syms.filter(ctx.typer.matchesSameStatic(replaced, _))
          for existing <- clashes do
            val src = syntheticDefs.origin
            report.error(i"""Unrolled $replaced clashes with existing declaration.
              |Please remove the clashing definition, or the @unroll annotation.
              |Unrolled from ${hl(src.showDcl)} ${symLocation(src)}""".stripMargin, existing.srcPos)
        }
    end if

    cpy.Template(tmpl)(
      tmpl.constr,
      tmpl.parents,
      tmpl.derived,
      tmpl.self,
      tmpl.body ++ allGenerated.flatMap(_.forwarders)
    )
  }

}

object UnrollDefinitions:
  val name: String = "unrollDefs"
  val description: String = "generates forwarders for methods annotated with @unroll"
