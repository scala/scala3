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

/**Implementation of SIP-61.
 * Runs when `@unroll` annotations are found in a compilation unit, installing new definitions
 */
class UnrollDefinitions extends MacroTransform, IdentityDenotTransformer {
  self =>

  import tpd.*

  private var _unrolledDefs: util.HashMap[Symbol, ComputedIndicies] | Null = null
  private def initializeUnrolledDefs(): util.HashMap[Symbol, ComputedIndicies] =
    val local = _unrolledDefs
    if local == null then
      val map = new util.HashMap[Symbol, ComputedIndicies]
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

  type ComputedIndicies = Seq[(Int, List[Int])]
  type ComputeIndicies = Context ?=> Symbol => ComputedIndicies

  private class UnrollingTransformer(classes: Set[Symbol]) extends Transformer {
    private val unrolledDefs = initializeUnrolledDefs()

    def computeIndices(annotated: Symbol)(using Context): ComputedIndicies =
      unrolledDefs.getOrElseUpdate(annotated, {
        annotated
          .paramSymss
          .zipWithIndex
          .flatMap { (paramClause, paramClauseIndex) =>
            val annotationIndices = findUnrollAnnotations(paramClause)
            if (annotationIndices.isEmpty) None
            else Some((paramClauseIndex, annotationIndices))
          }
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
                              isCaseApply: Boolean,
                              inferOverride: Boolean)(using Context) = {

    def initNewForwarder()(using Context): (TermSymbol, List[List[Symbol]]) = {
      val forwarderDefSymbol0 = Symbols.newSymbol(
        defdef.symbol.owner,
        defdef.name,
        (defdef.symbol.flags &~
        HasDefaultParams &~
        (if nextParamIndex == -1 then EmptyFlags else Deferred)) |
        Invisible | Synthetic |
        (if inferOverride then Override else EmptyFlags),
        NoType, // fill in later
        coord = nextSymbol.span.shift(1) // shift by 1 to avoid "secondary constructor must call preceding" error
      ).entered

      // we need this such that when unpickling a TERMREFdirect, if we see this annotation,
      // we restore the tree to a Select
      forwarderDefSymbol0.addAnnotation(defn.UnrollForwarderAnnot)

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
    val isDeferredInitial = paramCount == paramIndex && defdef.symbol.is(Deferred)

    val (forwarderDefSymbol, newParamSymLists) =
      if isDeferredInitial then
        val existing = defdef.symbol.asTerm
        existing.addAnnotation(defn.AbstractUnrollAnnot) // mark as previously abstract
        existing.flags = (existing.flags &~ Deferred) // going to implement its rhs
        existing -> extractParamSymss(identity)
      else
        initNewForwarder()

    if inferOverride then
      // in this case we will not replace the source method, but we will add the override flag
      defdef.symbol.flags_=(defdef.symbol.flags | Override)

    def forwarderRhs(): tpd.Tree = {
      val defaultOffset = defdef.paramss
        .iterator
        .take(annotatedParamListIndex)
        .filter(!isTypeClause(_))
        .map(_.size)
        .sum

      val defaultCalls = Range(paramIndex, nextParamIndex).map(n =>

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

      val forwarderInner: Tree = This(defdef.symbol.owner.asClass).select(nextSymbol)

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
      tpd.DefDef(forwarderDefSymbol,
        rhs = if nextParamIndex == -1 then EmptyTree else forwarderRhs())

    forwarderDef.withSpan(if isDeferredInitial then defdef.span else nextSymbol.span.shift(1))
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

  def generateSyntheticDefs(tree: Tree, compute: ComputeIndicies)(using Context): (Option[Symbol], Seq[(Symbol, Tree)]) = tree match {
    case defdef: DefDef if defdef.paramss.nonEmpty =>
      import dotty.tools.dotc.core.NameOps.isConstructorName

      // infer an override when we are implementing a method that matches the signature and has unroll annotations
      // in the same positions
      lazy val inferOverride = {
        def unrollIndices(sym: Symbol): List[Int] =
          sym.paramSymss.flatten.zipWithIndex.collect({
            case (p, i) if p.hasAnnotation(defn.UnrollAnnot) => i
          })

        val candidate = defdef.symbol.nextOverriddenSymbol
        candidate.exists && !candidate.is(Deferred) && candidate.hasAnnotation(defn.AbstractUnrollAnnot) && {
          // check unroll indices match
          unrollIndices(candidate) == unrollIndices(defdef.symbol)
        }
      }

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
        case Nil => (None, Nil)
        case Seq((paramClauseIndex, annotationIndices)) =>
          val paramCount = annotated.paramSymss(paramClauseIndex).size
          if (isCaseFromProduct) {
            val newDef = generateFromProduct(annotationIndices, paramCount, defdef)
            (Some(defdef.symbol), Seq(defdef.symbol -> newDef))
          } else {
            if (defdef.symbol.is(Deferred)){
              (
                Some(defdef.symbol),
                (-1 +: annotationIndices :+ paramCount).sliding(2).toList.foldLeft((Seq.empty[(Symbol, DefDef)], defdef.symbol))((m, v) => ((m, v): @unchecked) match {
                  case ((defdefs, nextSymbol), Seq(paramIndex, nextParamIndex)) =>
                    val forwarder = generateSingleForwarder(
                      defdef,
                      defdef.symbol.info,
                      nextParamIndex,
                      paramIndex,
                      nextSymbol,
                      paramClauseIndex,
                      isCaseApply,
                      inferOverride
                    )
                    // replacements += forwarder.symbol
                    ((defdef.symbol -> forwarder) +: defdefs, forwarder.symbol)
                })._1
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
                      isCaseApply,
                      inferOverride
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

  def unrollTemplate(tmpl: tpd.Template, compute: ComputeIndicies)(using Context): tpd.Tree = {

    val (removed0, generatedDefs0) = tmpl.body.map(generateSyntheticDefs(_, compute)).unzip
    val (removedCtor, generatedConstr0) = generateSyntheticDefs(tmpl.constr, compute)
    val removedSymsBody = removed0.flatten
    val allRemoved = removedSymsBody ++ removedCtor

    val generatedDefOrigins = generatedDefs0.flatten
    val generatedDefs = generatedDefOrigins.map(_(1))
    val generatedConstr = generatedConstr0.map(_(1))

    val otherDecls = tmpl.body.filter(t => !removedSymsBody.contains(t.symbol))

    /** inlined from compiler/src/dotty/tools/dotc/typer/Checking.scala */
    def checkClash(decl: Symbol, other: Symbol) =
      def staticNonStaticPair = decl.isScalaStatic != other.isScalaStatic
      decl.matches(other) && !staticNonStaticPair

    if generatedDefOrigins.nonEmpty then
      val byName = otherDecls.groupMap(_.symbol.name.toString)(_.symbol)
      for case (src, dcl: NamedDefTree) <- generatedDefOrigins do
        val replaced = dcl.symbol
        byName.get(dcl.name.toString).foreach { syms =>
          val clashes = syms.filter(checkClash(replaced, _))
          for existing <- clashes do
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
      otherDecls ++ generatedDefs ++ generatedConstr
    )
  }

}

object UnrollDefinitions:
  val name: String = "unrollDefs"
  val description: String = "generates forwarders for methods annotated with @unroll"
