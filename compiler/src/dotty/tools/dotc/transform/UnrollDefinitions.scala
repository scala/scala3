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

import dotty.tools.dotc.core.Types.{MethodType, NamedType, PolyType, Type, NoPrefix, NoType}

import dotty.tools.dotc.printing.Formatting.hl

import scala.collection.mutable
import scala.util.boundary, boundary.break
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

  private var _unrolledDefs: util.EqHashMap[Symbol, ComputedIndicies] | Null = null
  private def initializeUnrolledDefs(): util.EqHashMap[Symbol, ComputedIndicies] =
    val local = _unrolledDefs
    if local == null then
      val map = new util.EqHashMap[Symbol, ComputedIndicies]
      _unrolledDefs = map
      map
    else
      local.clear()
      local

  override def phaseName: String = UnrollDefinitions.name

  override def description: String = UnrollDefinitions.description

  override def changesMembers: Boolean = true

  override def run(using Context): Unit =
    if ctx.compilationUnit.hasUnrollDefs then
      super.run // create and run the transformer on the current compilation unit

  def newTransformer(using Context): Transformer =
    UnrollingTransformer(ctx.compilationUnit.unrolledClasses.nn)

  type ComputedIndicies = List[(Int, List[Int])]
  type ComputeIndicies = Context ?=> Symbol => ComputedIndicies

  private class UnrollingTransformer(classes: Set[Symbol]) extends Transformer {
    private val unrolledDefs = initializeUnrolledDefs()

    def computeIndices(annotated: Symbol)(using Context): ComputedIndicies =
      unrolledDefs.getOrElseUpdate(annotated, {
        if annotated.name.is(DefaultGetterName) then
          Nil // happens in curried methods where more than one parameter list has @unroll
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
            assert(annotated.is(Final, butNot = Deferred) || annotated.isConstructor || annotated.owner.is(ModuleClass) || annotated.name.is(DefaultGetterName),
              i"$annotated is not final&concrete, or a constructor")
          indices
      })
    end computeIndices

    override def transform(tree: tpd.Tree)(using Context): tpd.Tree = tree match
      case tree @ TypeDef(_, impl: Template) if classes(tree.symbol) =>
        super.transform(cpy.TypeDef(tree)(rhs = unrollTemplate(impl, computeIndices)))
      case tree =>
        super.transform(tree)
  }

  def copyParamSym(sym: Symbol, parent: Symbol)(using Context): (Symbol, Symbol) =
    val copied = sym.copy(owner = parent, flags = (sym.flags &~ HasDefault), coord = sym.coord)
    sym -> copied

  def symLocation(sym: Symbol)(using Context) = {
    val lineDesc =
      if (sym.span.exists && sym.span != sym.owner.span)
        s" at line ${sym.srcPos.line + 1}"
      else ""
    i"in ${sym.owner}${lineDesc}"
  }

  def findUnrollAnnotations(params: List[Symbol])(using Context): List[Int] = {
    params
      .zipWithIndex
      .collect {
        case (v, i) if v.hasAnnotation(defn.UnrollAnnot) =>
          i
      }
  }

  def isTypeClause(p: ParamClause) = p.headOption.exists(_.isInstanceOf[TypeDef])

  def generateSingleForwarder(defdef: DefDef,
                              prevMethodType: Type,
                              paramIndex: Int,
                              paramCount: Int,
                              nextParamIndex: Int,
                              nextSymbol: Symbol,
                              annotatedParamListIndex: Int,
                              isCaseApply: Boolean)(using Context) = {

    def initNewForwarder()(using Context): (TermSymbol, List[List[Symbol]]) = {
      val forwarderDefSymbol0 = Symbols.newSymbol(
        defdef.symbol.owner,
        defdef.name,
        defdef.symbol.flags &~ HasDefaultParams |
        Invisible | Synthetic,
        NoType, // fill in later
        coord = nextSymbol.span.shift(1) // shift by 1 to avoid "secondary constructor must call preceding" error
      ).entered

      val newParamSymMappings = extractParamSymss(copyParamSym(_, forwarderDefSymbol0))
      val (oldParams, newParams) = newParamSymMappings.flatten.unzip

      val newParamSymLists0 =
        newParamSymMappings.map: pairss =>
          pairss.map: (oldSym, newSym) =>
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

    val paramCount = defdef.symbol.paramSymss(annotatedParamListIndex).size

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
          .foldLeft(inner): (lhs, newParams) =>
            if (newParams.headOption.exists(_.isInstanceOf[TypeTree])) TypeApply(lhs, newParams)
            else Apply(lhs, newParams)
      )

      val forwarderInner: Tree =
        This(defdef.symbol.owner.asClass).select(defdef.symbol)

      val forwarderCallArgs =
        newParamSymLists.zipWithIndex.map{case (ps, i) =>
          if (i == annotatedParamListIndex) ps.map(ref).take(nextParamIndex) ++ defaultCalls
          else ps.map(ref)
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
      tpd.DefDef(forwarderDefSymbol, rhs = forwarderRhs())

    forwarderDef.withSpan(nextSymbol.span.shift(1))
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
            Underscore(defn.IntType),
            EmptyTree,
            defdef.rhs
          )
        )
      )
    ).setDefTree
  }

  def generateSyntheticDefs(tree: Tree, compute: ComputeIndicies)(using Context): Option[(Symbol, Option[Symbol], Seq[DefDef])] = tree match {
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

      compute(annotated) match {
        case Nil => None
        case Seq((paramClauseIndex, annotationIndices)) =>
          val paramCount = annotated.paramSymss(paramClauseIndex).size
          if isCaseFromProduct then
            Some((defdef.symbol, Some(defdef.symbol), Seq(generateFromProduct(annotationIndices, paramCount, defdef))))
          else
            val (generatedDefs, _) =
              val indices = (annotationIndices :+ paramCount).sliding(2).toList.reverse
              indices.foldLeft((Seq.empty[DefDef], defdef.symbol)):
                case ((defdefs, nextSymbol), Seq(paramIndex, nextParamIndex)) =>
                  val forwarder = generateSingleForwarder(
                    defdef,
                    defdef.symbol.info,
                    paramIndex,
                    paramCount,
                    nextParamIndex,
                    nextSymbol,
                    paramClauseIndex,
                    isCaseApply
                  )
                  (forwarder +: defdefs, forwarder.symbol)
                case _ => unreachable("sliding with at least 2 elements")
            Some((defdef.symbol, None, generatedDefs))

        case multiple =>
          report.error("Cannot have multiple parameter lists containing `@unroll` annotation", defdef.srcPos)
          None
      }

    case _ => None
  }

  def unrollTemplate(tmpl: tpd.Template, compute: ComputeIndicies)(using Context): tpd.Tree = {

    val generatedBody = tmpl.body.flatMap(generateSyntheticDefs(_, compute))
    val generatedConstr0 = generateSyntheticDefs(tmpl.constr, compute)
    val allGenerated = generatedBody ++ generatedConstr0
    val bodySubs = generatedBody.flatMap((_, maybeSub, _) => maybeSub).toSet
    val otherDecls = tmpl.body.filterNot(d => d.symbol.exists && bodySubs(d.symbol))

    /** inlined from compiler/src/dotty/tools/dotc/typer/Checking.scala */
    def checkClash(decl: Symbol, other: Symbol) =
      def staticNonStaticPair = decl.isScalaStatic != other.isScalaStatic
      decl.matches(other) && !staticNonStaticPair

    if allGenerated.nonEmpty then
      val byName = (tmpl.constr :: otherDecls).groupMap(_.symbol.name.toString)(_.symbol)
      for
        (src, _, dcls) <- allGenerated
        dcl <- dcls
      do
        val replaced = dcl.symbol
        byName.get(dcl.name.toString).foreach { syms =>
          val clashes = syms.filter(checkClash(replaced, _))
          for existing <- clashes do
            report.error(i"""Unrolled $replaced clashes with existing declaration.
              |Please remove the clashing definition, or the @unroll annotation.
              |Unrolled from ${hl(src.showDcl)} ${symLocation(src)}""".stripMargin, existing.srcPos)
        }
    end if

    val generatedDefs = generatedBody.flatMap((_, _, gens) => gens)
    val generatedConstr = generatedConstr0.toList.flatMap((_, _, gens) => gens)

    cpy.Template(tmpl)(
      tmpl.constr,
      tmpl.parents,
      tmpl.derived,
      tmpl.self,
      otherDecls ++ generatedDefs ++ generatedConstr
    )
  }

}

object UnrollDefinitions:
  val name: String = "unrollDefs"
  val description: String = "generates forwarders for methods annotated with @unroll"
