package dotty.tools
package dotc
package ast

import core.*
import util.Spans.*, Types.*, Contexts.*, Constants.*, Names.*, NameOps.*, Flags.*
import Symbols.*, StdNames.*, Trees.*, ContextOps.*
import Decorators.*
import Annotations.Annotation
import NameKinds.{UniqueName, ContextBoundParamName, ContextFunctionParamName, DefaultGetterName, WildcardParamName}
import typer.{Namer, Checking}
import util.{Property, SourceFile, SourcePosition, SrcPos, Chars}
import config.{Feature, Config}
import config.Feature.{sourceVersion, migrateTo3, enabled}
import config.SourceVersion.*
import collection.mutable, mutable.ListBuffer
import reporting.*
import printing.Formatting.hl
import config.Printers
import parsing.Parsers
import util.chaining.*

import scala.annotation.{unchecked as _, *}, internal.sharable

object desugar {
  import untpd.*
  import DesugarEnums.*

  /** An attachment for companion modules of classes that have a `derives` clause.
   *  The position value indicates the start position of the template of the
   *  deriving class.
   */
  val DerivingCompanion: Property.Key[SourcePosition] = Property.Key()

  /** An attachment for match expressions generated from a PatDef or GenFrom.
   *  Value of key == one of IrrefutablePatDef, IrrefutableGenFrom
   */
  val CheckIrrefutable: Property.Key[MatchCheck] = Property.StickyKey()

  /** A multi-line infix operation with the infix operator starting a new line.
   *  Used for explaining potential errors.
   */
  val MultiLineInfix: Property.Key[Unit] = Property.StickyKey()

  /** An attachment key to indicate that a ValDef originated from parameter untupling.
   */
  val UntupledParam: Property.Key[Unit] = Property.StickyKey()

  /** An attachment key to indicate that a ValDef originated from a pattern.
   */
  val PatternVar: Property.Key[Unit] = Property.StickyKey()

  /** An attachment key for Trees originating in for-comprehension, such as tupling of assignments.
   */
  val ForArtifact: Property.Key[Unit] = Property.StickyKey()

  /** An attachment key to indicate that a ValDef is an evidence parameter
   *  for a context bound.
   */
  val ContextBoundParam: Property.Key[Unit] = Property.StickyKey()

  /** Marks a poly fcuntion apply method, so that we can handle adding evidence parameters to them in a special way
   */
  val PolyFunctionApply: Property.Key[Unit] = Property.StickyKey()

  /** An attachment key to indicate that an Apply is created as a last `map`
   *  scall in a for-comprehension.
   */
  val TrailingForMap: Property.Key[Unit] = Property.StickyKey()

  val WasTypedInfix: Property.Key[Unit] = Property.StickyKey()

  /** What static check should be applied to a Match? */
  enum MatchCheck {
    case None, Exhaustive, IrrefutablePatDef, IrrefutableGenFrom
  }

  /** Is `name` the name of a method that can be invalidated as a compiler-generated
   *  case class method if it clashes with a user-defined method?
   */
  def isRetractableCaseClassMethodName(name: Name)(using Context): Boolean = name match {
    case nme.apply | nme.unapply | nme.unapplySeq | nme.copy => true
    case DefaultGetterName(nme.copy, _) => true
    case _ => false
  }

  /** Is `name` the name of a method that is added unconditionally to case classes? */
  def isDesugaredCaseClassMethodName(name: Name)(using Context): Boolean =
    isRetractableCaseClassMethodName(name) || name.isSelectorName

// ----- DerivedTypeTrees -----------------------------------

  class SetterParamTree(implicit @constructorOnly src: SourceFile) extends DerivedTypeTree {
    def derivedTree(sym: Symbol)(using Context): tpd.TypeTree = tpd.TypeTree(sym.info.resultType)
  }

  class TypeRefTree(implicit @constructorOnly src: SourceFile) extends DerivedTypeTree {
    def derivedTree(sym: Symbol)(using Context): tpd.TypeTree = tpd.TypeTree(sym.typeRef)
  }

  class TermRefTree(implicit @constructorOnly src: SourceFile) extends DerivedTypeTree {
    def derivedTree(sym: Symbol)(using Context): tpd.Tree = tpd.ref(sym)
  }

  /** A type tree that computes its type from an existing parameter. */
  class DerivedFromParamTree()(implicit @constructorOnly src: SourceFile) extends DerivedTypeTree {

    /** Complete the appropriate constructors so that OriginalSymbol attachments are
     *  pushed to DerivedTypeTrees.
     */
    override def ensureCompletions(using Context): Unit = {
      def completeConstructor(sym: Symbol) =
        sym.infoOrCompleter match {
          case completer: Namer#ClassCompleter if !sym.isCompleting =>
            // An example, derived from tests/run/t6385.scala
            //
            //     class Test():
            //       def t1: Foo = Foo(1)
            //     final case class Foo(value: Int)
            //
            // Here's the sequence of events:
            //  * The symbol for Foo.apply is forced to complete
            //  * The symbol for the `value` parameter of the apply method is forced to complete
            //  * Completing that value parameter requires typing its type, which is a DerivedTypeTrees,
            //    which only types if it has an OriginalSymbol.
            //  * So if the case class hasn't been completed, we need (at least) its constructor to be completed
            //
            // Test tests/neg/i9294.scala is an example of why isCompleting is necessary.
            // Annotations are added while completing the constructor,
            // so the back reference to foo reaches here which re-initiates the constructor completion.
            // So we just skip, as completion is already being triggered.
            completer.completeConstructor(sym)
          case _ =>
        }

      if (!ctx.owner.is(Package))
        if (ctx.owner.isClass) {
          completeConstructor(ctx.owner)
          if (ctx.owner.is(ModuleClass))
            completeConstructor(ctx.owner.linkedClass)
        }
        else ensureCompletions(using ctx.outer)
    }

    /** Return info of original symbol, where all references to siblings of the
     *  original symbol (i.e. sibling and original symbol have the same owner)
     *  are rewired to same-named parameters or accessors in the scope enclosing
     *  the current scope. The current scope is the scope owned by the defined symbol
     *  itself, that's why we have to look one scope further out. If the resulting
     *  type is an alias type, dealias it. This is necessary because the
     *  accessor of a type parameter is a private type alias that cannot be accessed
     *  from subclasses.
     */
    def derivedTree(sym: Symbol)(using Context): tpd.TypeTree =
      val relocate = new TypeMap:
        val originalOwner = sym.owner
        def apply(tp: Type) = tp match {
          case tp: NamedType if tp.symbol.exists && (tp.symbol.owner eq originalOwner) =>
            val defctx = mapCtx.outersIterator.dropWhile(_.scope eq mapCtx.scope).next()
            var local = defctx.denotNamed(tp.name).suchThat(_.isParamOrAccessor).symbol
            if local.exists then defctx.owner.thisType.select(local).dealiasKeepAnnots
            else
              def msg =
                em"no matching symbol for ${tp.symbol.showLocated} in ${defctx.owner} / ${defctx.effectiveScope.toList}"
              ErrorType(msg).assertingErrorsReported(msg)
          case _ =>
            mapOver(tp)
        }
      tpd.TypeTree(relocate(sym.info))
  }

  /** A type definition copied from `tdef` with a rhs typetree derived from it */
  def derivedTypeParam(tdef: TypeDef)(using Context): TypeDef =
    cpy.TypeDef(tdef)(
      rhs = DerivedFromParamTree().withSpan(tdef.rhs.span).watching(tdef)
    )

  /** A derived type definition watching `sym` */
  def derivedTypeParamWithVariance(sym: TypeSymbol)(using Context): TypeDef =
    val variance = VarianceFlags & sym.flags
    TypeDef(sym.name, DerivedFromParamTree().watching(sym)).withFlags(TypeParam | Synthetic | variance)

  /** A value definition copied from `vdef` with a tpt typetree derived from it */
  def derivedTermParam(vdef: ValDef)(using Context): ValDef =
    derivedTermParam(vdef, vdef.unforcedRhs)

  def derivedTermParam(vdef: ValDef, rhs: LazyTree)(using Context): ValDef =
    cpy.ValDef(vdef)(
      tpt = DerivedFromParamTree().withSpan(vdef.tpt.span).watching(vdef),
      rhs = rhs
    )

// ----- Desugar methods -------------------------------------------------

  /** Setter generation is needed for:
   *    - non-private class members
   *    - all trait members
   *    - all package object members
   */
  def isSetterNeeded(valDef: ValDef)(using Context): Boolean = {
    val mods = valDef.mods
    mods.is(Mutable)
      && ctx.owner.isClass
      && (!mods.is(Private) || ctx.owner.is(Trait) || ctx.owner.isPackageObject)
  }

  /**   var x: Int = expr
   *  ==>
   *    def x: Int = expr
   *    def x_=($1: <TypeTree()>): Unit = ()
   *
   *  Generate setter where needed
   */
  def valDef(vdef0: ValDef)(using Context): Tree =
    val vdef @ ValDef(_, tpt, rhs) = vdef0
    val valName = normalizeName(vdef, tpt).asTermName
    var mods1 = vdef.mods

    val vdef1 = cpy.ValDef(vdef)(name = valName).withMods(mods1)

    if isSetterNeeded(vdef) then
      val setterParam = makeSyntheticParameter(tpt = SetterParamTree().watching(vdef))
      // The rhs gets filled in later, when field is generated and getter has parameters (see Memoize miniphase)
      val setterRhs = if (vdef.rhs.isEmpty) EmptyTree else syntheticUnitLiteral
      val setter = cpy.DefDef(vdef)(
          name    = valName.setterName,
          paramss = (setterParam :: Nil) :: Nil,
          tpt     = TypeTree(defn.UnitType),
          rhs     = setterRhs
        ).withMods((vdef.mods | Accessor) &~ (CaseAccessor | GivenOrImplicit | Lazy | Transparent))
        .dropEndMarker() // the end marker should only appear on the getter definition
      Thicket(vdef1, setter)
    else vdef1
  end valDef

  def mapParamss(paramss: List[ParamClause])
                (mapTypeParam: TypeDef => TypeDef)
                (mapTermParam: ValDef => ValDef)(using Context): List[ParamClause] =
    paramss.mapConserve {
      case TypeDefs(tparams) => tparams.mapConserve(mapTypeParam)
      case ValDefs(vparams) => vparams.mapConserve(mapTermParam)
      case _ => unreachable()
    }

  /** 1. Expand context bounds to evidence params. E.g.,
   *
   *      def f[T >: L <: H : B](params)
   *  ==>
   *      def f[T >: L <: H](params)(implicit evidence$0: B[T])
   *
   *  2. Expand default arguments to default getters. E.g,
   *
   *      def f[T: B](x: Int = 1)(y: String = x + "m") = ...
   *  ==>
   *      def f[T](x: Int)(y: String)(implicit evidence$0: B[T]) = ...
   *      def f$default$1[T] = 1
   *      def f$default$2[T](x: Int) = x + "m"
   */
  private def defDef(meth: DefDef, isPrimaryConstructor: Boolean = false)(using Context): Tree =
    addDefaultGetters(elimContextBounds(meth, isPrimaryConstructor).asInstanceOf[DefDef])

  /** Drop context bounds in given TypeDef, replacing them with evidence ValDefs that
   *  get added to a buffer.
   *  @param tdef          The given TypeDef
   *  @param evidenceBuf   The buffer to which evidence gets added. This buffer
   *                       is shared between desugarings of different type parameters
   *                       of the same method.
   *  @param evidenceFlags The flags to use for evidence definitions
   *  @param freshName     A function to generate fresh names for evidence definitions
   *  @param allParamss    If `tdef` is a type paramter, all parameters of the owning method,
   *                       otherwise the empty list.
   */
  private def desugarContextBounds(
      tdef: TypeDef,
      evidenceBuf: ListBuffer[ValDef],
      evidenceFlags: FlagSet,
      freshName: untpd.Tree => TermName,
      allParamss: List[ParamClause])(using Context): TypeDef =

    val evidenceNames = ListBuffer.empty[TermName]

    def desugarRHS(rhs: Tree): Tree = rhs match
      case ContextBounds(tbounds, ctxbounds) =>
        val isMember = evidenceFlags.isAllOf(DeferredGivenFlags)
        for bound <- ctxbounds do
          val (evidenceName, spanPoint) = bound match
            case ContextBoundTypeTree(_, _, ownName) if !ownName.isEmpty =>
              val realName = ownName.stripModuleClassSuffix.lastPart
              (ownName, bound.span.end - realName.length) // if there is an explicitly given name, use it.
            case _ =>
              if Config.nameSingleContextBounds
                && !isMember
                && ctxbounds.tail.isEmpty
                && Feature.enabled(Feature.modularity)
              then (tdef.name.toTermName, bound.span.point)
              else (freshName(bound), bound.span.point)
          evidenceNames += evidenceName
          val evidenceParam =
            ValDef(evidenceName, bound, EmptyTree).withFlags(evidenceFlags).withSpan(bound.span.withPoint(spanPoint))
          evidenceParam.pushAttachment(ContextBoundParam, ())
          evidenceBuf += evidenceParam
        tbounds
      case LambdaTypeTree(tparams, body) =>
        cpy.LambdaTypeTree(rhs)(tparams, desugarRHS(body))
      case _ =>
        rhs

    val tdef1 = cpy.TypeDef(tdef)(rhs = desugarRHS(tdef.rhs))
    // Under x.modularity, if there was a context bound, and `tdef`s name as a term name is
    // neither a name of an existing parameter nor a name of generated evidence for
    // the same method, add a WitnessAnnotation with all generated evidence names to `tdef`.
    // This means a context bound proxy will be created later.
    if Feature.enabled(Feature.modularity)
        && evidenceNames.nonEmpty
        && !evidenceBuf.exists(_.name == tdef.name.toTermName)
        && !allParamss.nestedExists(_.name == tdef.name.toTermName)
    then
      tdef1.withAddedAnnotation:
        WitnessNamesAnnot(evidenceNames.toList).withSpan(tdef.span)
    else
      tdef1
  end desugarContextBounds

  def elimContextBounds(meth: Tree, isPrimaryConstructor: Boolean = false)(using Context): Tree =
    val evidenceParamBuf = ListBuffer.empty[ValDef]
    var seenContextBounds: Int = 0
    def freshName(unused: Tree) =
      seenContextBounds += 1 // Start at 1 like FreshNameCreator.
      ContextBoundParamName(EmptyTermName, seenContextBounds)
      // Just like with `makeSyntheticParameter` on nameless parameters of
      // using clauses, we only need names that are unique among the
      // parameters of the method since shadowing does not affect
      // implicit resolution in Scala 3.

    def paramssNoContextBounds(paramss: List[ParamClause]): List[ParamClause] =
      val iflag = paramss.lastOption.flatMap(_.headOption) match
        case Some(param) if param.mods.isOneOf(GivenOrImplicit) =>
          param.mods.flags & GivenOrImplicit
        case _ =>
          if Feature.sourceVersion.isAtLeast(`3.6`) then Given
          else Implicit
      val flags = if isPrimaryConstructor then iflag | LocalParamAccessor else iflag | Param
      mapParamss(paramss) {
        tparam => desugarContextBounds(tparam, evidenceParamBuf, flags, freshName, paramss)
      }(identity)

    meth match
      case meth @ DefDef(_, paramss, tpt, rhs) =>
        val newParamss = paramssNoContextBounds(paramss)
        rhs match
          case MacroTree(call) =>
            cpy.DefDef(meth)(rhs = call).withMods(meth.mods | Macro | Erased)
          case _ =>
            addEvidenceParams(
              cpy.DefDef(meth)(
                name = normalizeName(meth, tpt).asTermName,
                paramss = newParamss
              ),
              evidenceParamBuf.toList
            )
      case meth @ PolyFunction(tparams, fun) =>
        val PolyFunction(tparams: List[untpd.TypeDef] @unchecked, fun) = meth: @unchecked
        val Function(vparams: List[untpd.ValDef] @unchecked, rhs) = fun: @unchecked
        val newParamss = paramssNoContextBounds(tparams :: vparams :: Nil)
        val params = evidenceParamBuf.toList
        if params.isEmpty then
          meth
        else
          val boundNames = getBoundNames(params, newParamss)
          val recur = fitEvidenceParams(params, nme.apply, boundNames)
          val (paramsFst, paramsSnd) = recur(newParamss)
          functionsOf((paramsFst ++ paramsSnd).filter(_.nonEmpty), rhs)
  end elimContextBounds

  def addDefaultGetters(meth: DefDef)(using Context): Tree =

    /** The longest prefix of parameter lists in paramss whose total number of
     *  ValDefs does not exceed `n`
     */
    def takeUpTo(paramss: List[ParamClause], n: Int): List[ParamClause] = paramss match
      case ValDefs(vparams) :: paramss1 =>
        val len = vparams.length
        if len <= n then vparams :: takeUpTo(paramss1, n - len) else Nil
      case TypeDefs(tparams) :: paramss1 =>
        tparams :: takeUpTo(paramss1, n)
      case _ =>
        Nil

    def dropContextBounds(tparam: TypeDef): TypeDef =
      def dropInRhs(rhs: Tree): Tree = rhs match
        case ContextBounds(tbounds, _) =>
          tbounds
        case rhs @ LambdaTypeTree(tparams, body) =>
          cpy.LambdaTypeTree(rhs)(tparams, dropInRhs(body))
        case _ =>
          rhs
      cpy.TypeDef(tparam)(rhs = dropInRhs(tparam.rhs))

    def paramssNoRHS = mapParamss(meth.paramss)(identity) {
      vparam =>
        if vparam.rhs.isEmpty then vparam
        else cpy.ValDef(vparam)(rhs = EmptyTree).withMods(vparam.mods | HasDefault)
    }

    def getterParamss(n: Int): List[ParamClause] =
      mapParamss(takeUpTo(paramssNoRHS, n)) {
        tparam => dropContextBounds(toMethParam(tparam, KeepAnnotations.All))
      } {
        vparam => toMethParam(vparam, KeepAnnotations.All, keepDefault = false)
      }

    def defaultGetters(paramss: List[ParamClause], n: Int): List[DefDef] = paramss match
      case ValDefs(vparam :: vparams) :: paramss1 =>
        def defaultGetter: DefDef =
          DefDef(
            name = DefaultGetterName(meth.name, n),
            paramss = getterParamss(n),
            tpt = TypeTree(),
            rhs = vparam.rhs
          )
          .withMods(Modifiers(
            meth.mods.flags & (AccessFlags | Synthetic) | (vparam.mods.flags & Inline),
            meth.mods.privateWithin))
          .withSpan(vparam.rhs.span)
        val rest = defaultGetters(vparams :: paramss1, n + 1)
        if vparam.rhs.isEmpty then rest else defaultGetter :: rest
      case _ :: paramss1 =>  // skip empty parameter lists and type parameters
        defaultGetters(paramss1, n)
      case Nil =>
        Nil

    val defGetters = defaultGetters(meth.paramss, 0)
    if defGetters.isEmpty then meth
    else Thicket(cpy.DefDef(meth)(paramss = paramssNoRHS) :: defGetters)
  end addDefaultGetters

  /** Add an explicit ascription to the `expectedTpt` to every tail splice.
   *
   *  - `'{ x }` -> `'{ x }`
   *  - `'{ $x }` -> `'{ $x: T }`
   *  - `'{ if (...) $x else $y }` -> `'{ if (...) ($x: T) else ($y: T) }`
   *
   *  Note that the splice `$t: T` will be typed as `${t: Expr[T]}`
   */
  def quotedPattern(tree: untpd.Tree, expectedTpt: untpd.Tree)(using Context): untpd.Tree = {
    def adaptToExpectedTpt(tree: untpd.Tree): untpd.Tree = tree match {
      // Add the expected type as an ascription
      case _: untpd.SplicePattern =>
        untpd.Typed(tree, expectedTpt).withSpan(tree.span)
      case Typed(expr: untpd.SplicePattern, tpt) =>
        cpy.Typed(tree)(expr, untpd.makeAndType(tpt, expectedTpt).withSpan(tpt.span))

      // Propagate down the expected type to the leafs of the expression
      case Block(stats, expr) =>
        cpy.Block(tree)(stats, adaptToExpectedTpt(expr))
      case If(cond, thenp, elsep) =>
        cpy.If(tree)(cond, adaptToExpectedTpt(thenp), adaptToExpectedTpt(elsep))
      case untpd.Parens(expr) =>
        cpy.Parens(tree)(adaptToExpectedTpt(expr))
      case Match(selector, cases) =>
        val newCases = cases.map(cdef => cpy.CaseDef(cdef)(body = adaptToExpectedTpt(cdef.body)))
        cpy.Match(tree)(selector, newCases)
      case untpd.ParsedTry(expr, handler, finalizer) =>
        cpy.ParsedTry(tree)(adaptToExpectedTpt(expr), adaptToExpectedTpt(handler), finalizer)

      // Tree does not need to be ascribed
      case _ =>
        tree
    }
    adaptToExpectedTpt(tree)
  }

  /** Split out the quoted pattern type variable definition from the pattern.
   *
   *  Type variable definitions are all the `type t` defined at the start of a quoted pattern.
   *  Where name `t` is a pattern type variable name (i.e. lower case letters).
   *
   *  ```
   *   type t1; ...; type tn; <pattern>
   *  ```
   *  is split into
   *  ```
   *   (List(<type t1>; ...; <type tn>), <pattern>)
   *  ```
   */
  def quotedPatternTypeVariables(tree: untpd.Tree)(using Context): (List[untpd.TypeDef], untpd.Tree) =
    tree match
      case untpd.Block(stats, expr) =>
        val (untpdTypeVariables, otherStats) = stats.span {
          case tdef @ untpd.TypeDef(name, _) => !tdef.isBackquoted && name.isVarPattern
          case _ => false
        }
        val untpdCaseTypeVariables = untpdTypeVariables.asInstanceOf[List[untpd.TypeDef]].map {
          tdef => tdef.withMods(tdef.mods | Case)
        }
        val pattern = if otherStats.isEmpty then expr else untpd.cpy.Block(tree)(otherStats, expr)
        (untpdCaseTypeVariables, pattern)
      case _ =>
        (Nil, tree)

  private def referencesName(vdef: ValDef, names: Set[TermName])(using Context): Boolean =
    vdef.tpt.existsSubTree:
      case Ident(name: TermName) => names.contains(name)
      case _ => false

  /** Fit evidence `params` into the `mparamss` parameter lists, making sure
   * that all parameters referencing `params` are after them.
   * - for methods the final parameter lists are := result._1 ++ result._2
   * - for poly functions, each element of the pair contains at most one term
   *   parameter list
   *
   * @param params the evidence parameters list that should fit into `mparamss`
   * @param methName the name of the method that `mparamss` belongs to
   * @param boundNames the names of the evidence parameters
   * @param mparamss the original parameter lists of the method
   * @return a pair of parameter lists containing all parameter lists in a
   * reference-correct order; make sure that `params` is always at the
   * intersection of the pair elements; this is relevant, for poly functions
   * where `mparamss` is guaranteed to have exectly one term parameter list,
   * then each pair element will have at most one term parameter list
   */
  private def fitEvidenceParams(
    params: List[ValDef],
    methName: Name,
    boundNames: Set[TermName]
  )(mparamss: List[ParamClause])(using Context): (List[ParamClause], List[ParamClause]) = mparamss match
    case ValDefs(mparams) :: _ if mparams.exists(referencesName(_, boundNames)) =>
      (params :: Nil) -> mparamss
    case ValDefs(mparams @ (mparam :: _)) :: Nil if mparam.mods.isOneOf(GivenOrImplicit) =>
      val normParams =
        if params.head.mods.flags.is(Given) != mparam.mods.flags.is(Given) then
          params.map: param =>
            val normFlags = param.mods.flags &~ GivenOrImplicit | (mparam.mods.flags & (GivenOrImplicit))
            param.withMods(param.mods.withFlags(normFlags))
              .showing(i"adapted param $result ${result.mods.flags} for ${methName}", Printers.desugar)
        else params
      ((normParams ++ mparams) :: Nil) -> Nil
    case mparams :: mparamss1 =>
      val (fst, snd) = fitEvidenceParams(params, methName, boundNames)(mparamss1)
      (mparams :: fst) -> snd
    case Nil =>
      Nil -> (params :: Nil)

  /** Create a chain of possibly contextual functions from the parameter lists */
  private def functionsOf(paramss: List[ParamClause], rhs: Tree)(using Context): Tree = paramss match
    case Nil => rhs
    case ValDefs(head @ (fst :: _)) :: rest if fst.mods.isOneOf(GivenOrImplicit) =>
      val paramTpts = head.map(_.tpt)
      val paramNames = head.map(_.name)
      val paramsErased = head.map(_.mods.flags.is(Erased))
      makeContextualFunction(paramTpts, paramNames, functionsOf(rest, rhs), paramsErased).withSpan(rhs.span)
    case ValDefs(head) :: rest =>
      Function(head, functionsOf(rest, rhs))
    case TypeDefs(head) :: rest =>
      PolyFunction(head, functionsOf(rest, rhs))
    case _ =>
      assert(false, i"unexpected paramss $paramss")
      EmptyTree

  private def getBoundNames(params: List[ValDef], paramss: List[ParamClause])(using Context): Set[TermName] =
    var boundNames = params.map(_.name).toSet // all evidence parameter + context bound proxy names
    for mparams <- paramss; mparam <- mparams do
      mparam match
        case tparam: TypeDef if tparam.mods.annotations.exists(WitnessNamesAnnot.unapply(_).isDefined) =>
          boundNames += tparam.name.toTermName
        case _ =>
    boundNames

  /**  Add all evidence parameters in `params` as implicit parameters to `meth`.
   *   The position of the added parameters is determined as follows:
   *
   *     - If there is an existing parameter list that refers to one of the added
   *       parameters or their future context bound proxies in one of its parameter
   *       types, add the new parameters in front of the first such parameter list.
   *     - Otherwise, if the last parameter list consists of implicit or using parameters,
   *       join the new parameters in front of this parameter list, creating one
   *       parameter list (this is equivalent to Scala 2's scheme).
   *     - Otherwise, add the new parameter list at the end as a separate parameter clause.
   */
  private def addEvidenceParams(meth: DefDef, params: List[ValDef])(using Context): DefDef =
    if params.isEmpty then return meth

    val boundNames = getBoundNames(params, meth.paramss)

    val fitParams = fitEvidenceParams(params, meth.name, boundNames)

    if meth.removeAttachment(PolyFunctionApply).isDefined then
      // for PolyFunctions we are limited to a single term param list, so we
      // reuse the fitEvidenceParams logic to compute the new parameter lists
      // and then we add the other parameter lists as function types to the
      // return type
      val (paramsFst, paramsSnd) = fitParams(meth.paramss)
      if ctx.mode.is(Mode.Type) then
        cpy.DefDef(meth)(paramss = paramsFst, tpt = functionsOf(paramsSnd, meth.tpt))
      else
        cpy.DefDef(meth)(paramss = paramsFst, rhs = functionsOf(paramsSnd, meth.rhs))
    else
      val (paramsFst, paramsSnd) = fitParams(meth.paramss)
      cpy.DefDef(meth)(paramss = paramsFst ++ paramsSnd)
  end addEvidenceParams

  /** The parameters generated from the contextual bounds of `meth`, as generated by `desugar.defDef` */
  private def evidenceParams(meth: DefDef)(using Context): List[ValDef] =
    for
      case ValDefs(vparams @ (vparam :: _)) <- meth.paramss
      if vparam.mods.isOneOf(GivenOrImplicit)
      param <- vparams.takeWhile(_.hasAttachment(ContextBoundParam))
    yield
      param

  @sharable private val synthetic = Modifiers(Synthetic)

  /** Which annotations to keep in derived parameters */
  private enum KeepAnnotations:
    case None, All, WitnessOnly

  /** Filter annotations in `mods` according to `keep` */
  private def filterAnnots(mods: Modifiers, keep: KeepAnnotations)(using Context) = keep match
    case KeepAnnotations.None => mods.withAnnotations(Nil)
    case KeepAnnotations.All => mods
    case KeepAnnotations.WitnessOnly =>
      mods.withAnnotations:
        mods.annotations.filter:
          case WitnessNamesAnnot(_) => true
          case _ => false

  /** Map type parameter accessor to corresponding method (i.e. constructor) parameter */
  private def toMethParam(tparam: TypeDef, keep: KeepAnnotations)(using Context): TypeDef =
    val mods = filterAnnots(tparam.rawMods, keep)
    tparam.withMods(mods & EmptyFlags | Param)

  /** Map term parameter accessor to corresponding method (i.e. constructor) parameter */
  private def toMethParam(vparam: ValDef, keep: KeepAnnotations, keepDefault: Boolean)(using Context): ValDef = {
    val mods = filterAnnots(vparam.rawMods, keep)
    val hasDefault = if keepDefault then HasDefault else EmptyFlags
    // Need to ensure that tree is duplicated since term parameters can be watched
    // and cloning a term parameter will copy its watchers to the clone, which means
    // we'd get cross-talk between the original parameter and the clone.
    ValDef(vparam.name, vparam.tpt, vparam.rhs)
      .withSpan(vparam.span)
      .withAttachmentsFrom(vparam)
      .withMods(mods & (GivenOrImplicit | Erased | hasDefault | Tracked) | Param)
  }

  /** Desugar type def (not param): Under x.modularity this can expand
   *  context bounds, which are expanded to evidence ValDefs. These will
   *  ultimately map to deferred givens.
   */
  def typeDef(tdef: TypeDef)(using Context): Tree =
    val evidenceBuf = ListBuffer.empty[ValDef]
    val result = desugarContextBounds(
        tdef, evidenceBuf,
        (tdef.mods.flags.toTermFlags & AccessFlags) | Lazy | DeferredGivenFlags,
        inventGivenName, Nil)
    if tdef.mods.flags.is(Into, butNot = Opaque) then
      report.error(ModifierNotAllowedForDefinition(Into), flagSourcePos(tdef, Into))
    if evidenceBuf.isEmpty then result else Thicket(result :: evidenceBuf.toList)

  /** The expansion of a class definition. See inline comments for what is involved */
  def classDef(cdef: TypeDef)(using Context): Tree = {
    val impl @ Template(constr0, _, self, _) = cdef.rhs: @unchecked
    val className = normalizeName(cdef, impl).asTypeName
    val parents = impl.parents
    val (implDerived, implUses) = impl.derived.partition(getRetainsAnnot(_).isEmpty)
    val mods = cdef.mods
    val companionMods = mods
        .withFlags((mods.flags & (AccessFlags | Final)).toCommonFlags)
        .withMods(Nil)
        .withAnnotations(Nil)

    var defaultGetters: List[Tree] = Nil

    /** If there is a uses or uses_init clause, convert it to a @retains annotation
     *  and add it to `mods`.
     *  @param  idx  the index in implUses that describes the clause as a CapSet^{...}
     *               reference: 0 for `uses...`, 1 for `uses_init...`
     */
    def addImplUse(mods: Modifiers, idx: 0 | 1): Modifiers =
      if implUses.nonEmpty then
        val retains = getRetainsAnnot(implUses(idx))
        if isEmptyRetainsAnnot(retains) then mods
        else mods.withAddedAnnotation(retains)
      else mods

    def decompose(ddef: Tree): DefDef = ddef match {
      case meth: DefDef => meth
      case Thicket((meth: DefDef) :: defaults) =>
        defaultGetters = defaults
        meth
    }

    val constr1 = decompose(defDef(impl.constr, isPrimaryConstructor = true))

    // The original type and value parameters in the constructor already have the flags
    // needed to be type members (i.e. param, and possibly also private and local unless
    // prefixed by type or val). `tparams` and `vparamss` are the type parameters that
    // go in `constr`, the constructor after desugaring.

    /** Does `tree' look like a reference to AnyVal? Temporary test before we have inline classes */
    def isAnyVal(tree: Tree): Boolean = tree match {
      case Ident(tpnme.AnyVal) => true
      case Select(qual, tpnme.AnyVal) => isScala(qual)
      case _ => false
    }
    def isScala(tree: Tree): Boolean = tree match {
      case Ident(nme.scala) => true
      case Select(Ident(nme.ROOTPKG), nme.scala) => true
      case _ => false
    }

    def namePos = cdef.sourcePos.withSpan(cdef.nameSpan)

    val isObject = mods.is(Module)
    val isCaseClass  = mods.is(Case) && !isObject
    val isCaseObject = mods.is(Case) && isObject
    val isEnum = mods.isEnumClass && !mods.is(Module)
    def isEnumCase = mods.isEnumCase
    def isNonEnumCase = !isEnumCase && (isCaseClass || isCaseObject)
    val isValueClass = parents.nonEmpty && isAnyVal(parents.head)
      // This is not watertight, but `extends AnyVal` will be replaced by `inline` later.
    val caseClassInScala2Library = isCaseClass && Feature.shouldBehaveAsScala2

    val originalTparams = constr1.leadingTypeParams
    val originalVparamss = asTermOnly(constr1.trailingParamss)
    lazy val derivedEnumParams = enumClass.typeParams.map(derivedTypeParamWithVariance)
    val enumTParams =
      if isEnumCase then
        val tparamReferenced = typeParamIsReferenced(
          enumClass.typeParams, originalTparams, originalVparamss, parents)
        if originalTparams.isEmpty && (parents.isEmpty || tparamReferenced) then
          derivedEnumParams.map(tdef => tdef.withFlags(tdef.mods.flags | PrivateLocal))
        else Nil
      else Nil
    val impliedTparams = enumTParams ++ originalTparams

    if mods.is(Trait) then
      for vparams <- originalVparamss; vparam <- vparams do
        if isByNameType(vparam.tpt) then
          report.error(em"implementation restriction: traits cannot have by name parameters", vparam.srcPos)

    // Annotations on class _type_ parameters are set on the derived parameters
    // but not on the constructor parameters. The reverse is true for
    // annotations on class _value_ parameters.
    val constrTparams = impliedTparams.map(toMethParam(_, KeepAnnotations.WitnessOnly))
    val constrVparamss =
      if (originalVparamss.isEmpty) { // ensure parameter list is non-empty
        if (isCaseClass)
          report.error(CaseClassMissingParamList(cdef), namePos)
        ListOfNil
      }
      else if (isCaseClass && originalVparamss.head.exists(_.mods.isOneOf(GivenOrImplicit))) {
        report.error(CaseClassMissingNonImplicitParamList(cdef), namePos)
        ListOfNil
      }
      else originalVparamss.nestedMap(toMethParam(_, KeepAnnotations.All, keepDefault = true))
    val derivedTparams =
      constrTparams.zipWithConserve(impliedTparams)((tparam, impliedParam) =>
        derivedTypeParam(tparam).withAnnotations(impliedParam.mods.annotations))
    val derivedVparamss =
      constrVparamss.nestedMap: vparam =>
        val derived =
          if ctx.compilationUnit.isJava then derivedTermParam(vparam, Parsers.unimplementedExpr)
          else derivedTermParam(vparam)
        derived.withAnnotations(Nil)

    val constr = cpy.DefDef(constr1)(paramss = joinParams(constrTparams, constrVparamss))
      .withMods(addImplUse(constr1.mods, 1))

    if enumTParams.nonEmpty then
      defaultGetters = defaultGetters.map:
        case ddef: DefDef =>
          val tParams = enumTParams.map(tparam => toMethParam(tparam, KeepAnnotations.All))
          cpy.DefDef(ddef)(paramss = joinParams(tParams, ddef.trailingParamss))

    val (normalizedBody, enumCases, enumCompanionRef) = {
      // Add constructor type parameters and evidence implicit parameters
      // to auxiliary constructors; set defaultGetters as a side effect.
      def expandConstructor(tree: Tree) = tree match {
        case ddef: DefDef if ddef.name.isConstructorName =>
          decompose(
            defDef(
              addEvidenceParams(
                cpy.DefDef(ddef)(paramss = joinParams(constrTparams, ddef.paramss)),
                evidenceParams(constr1).map(toMethParam(_, KeepAnnotations.None, keepDefault = false)))))
        case stat =>
          stat
      }
      // The Identifiers defined by a case
      def caseIds(tree: Tree): List[Ident] = tree match {
        case tree: MemberDef => Ident(tree.name.toTermName) :: Nil
        case PatDef(_, ids: List[Ident] @ unchecked, _, _) => ids
      }

      val stats0 = impl.body.map(expandConstructor)
      val stats =
        if (ctx.owner eq defn.ScalaPackageClass) && defn.hasProblematicGetClass(className) then
          stats0.filterConserve {
            case ddef: DefDef =>
              ddef.name ne nme.getClass_
            case _ =>
              true
          }
        else
          stats0

      if (isEnum) {
        val (enumCases, enumStats) = stats.partition(DesugarEnums.isEnumCase)
        if (enumCases.isEmpty)
          report.error(EnumerationsShouldNotBeEmpty(cdef), namePos)
        else
          enumCases.last.pushAttachment(DesugarEnums.DefinesEnumLookupMethods, ())
        val enumCompanionRef = TermRefTree()
        val enumImport =
          Import(enumCompanionRef, enumCases.flatMap(caseIds).map(
            enumCase =>
              ImportSelector(enumCase.withSpan(enumCase.span.startPos))
            )
          )
        (enumImport :: enumStats, enumCases, enumCompanionRef)
      }
      else (stats, Nil, EmptyTree)
    }

    def anyRef = ref(defn.AnyRefAlias.typeRef)

    val arity = constrVparamss.head.length

    val classTycon: Tree = TypeRefTree() // watching is set at end of method

    def appliedTypeTree(tycon: Tree, args: List[Tree]) =
      (if (args.isEmpty) tycon else AppliedTypeTree(tycon, args))
        .withSpan(cdef.span.startPos)

    def isHK(tparam: Tree): Boolean = tparam match {
      case TypeDef(_, LambdaTypeTree(tparams, body)) => true
      case TypeDef(_, rhs: DerivedTypeTree) => isHK(rhs.watched)
      case _ => false
    }

    /** Is this a repeated argument x* (using a spread operator)? */
    def isRepeated(tree: Tree): Boolean = stripByNameType(tree) match
      case PostfixOp(_, Ident(tpnme.raw.STAR)) => true
      case _ => false

    def appliedRef(tycon: Tree, tparams: List[TypeDef] = constrTparams, widenHK: Boolean = false) = {
      val targs = for (tparam <- tparams) yield {
        val targ = refOfDef(tparam)
        def fullyApplied(tparam: Tree): Tree = tparam match {
          case TypeDef(_, LambdaTypeTree(tparams, body)) =>
            AppliedTypeTree(targ, tparams.map(_ => WildcardTypeBoundsTree()))
          case TypeDef(_, rhs: DerivedTypeTree) =>
            fullyApplied(rhs.watched)
          case _ =>
            targ
        }
        if (widenHK) fullyApplied(tparam) else targ
      }
      appliedTypeTree(tycon, targs)
    }

    // a reference to the class type bound by `cdef`, with type parameters coming from the constructor
    val classTypeRef = appliedRef(classTycon)

    // a reference to `enumClass`, with type parameters coming from the case constructor
    lazy val enumClassTypeRef =
      if (enumClass.typeParams.isEmpty)
        enumClassRef
      else if (originalTparams.isEmpty)
        appliedRef(enumClassRef)
      else {
        report.error(TypedCaseDoesNotExplicitlyExtendTypedEnum(enumClass, cdef)
            , cdef.srcPos.startPos)
        appliedTypeTree(enumClassRef, constrTparams map (_ => anyRef))
      }

    // new C[Ts](paramss)
    lazy val creatorExpr =
      val vparamss = constrVparamss match
        case (vparam :: _) :: _ if vparam.mods.is(Implicit) => // add a leading () to match class parameters
          Nil :: constrVparamss
        case _ =>
          if constrVparamss.nonEmpty && constrVparamss.forall {
            case vparam :: _ => vparam.mods.is(Given)
            case _ => false
          }
          then constrVparamss :+ Nil // add a trailing () to match class parameters
          else constrVparamss
      val nu = vparamss.foldLeft(makeNew(classTypeRef)) { (nu, vparams) =>
        val app = Apply(nu, vparams.map(refOfDef))
        vparams match {
          case vparam :: _ if vparam.mods.isOneOf(GivenOrImplicit) || vparam.name.is(ContextBoundParamName) =>
            app.setApplyKind(ApplyKind.Using)
          case _ => app
        }
      }
      ensureApplied(nu)

    val copiedAccessFlags = if Feature.migrateTo3 then EmptyFlags else AccessFlags

    // Methods to add to a case class C[..](p1: T1, ..., pN: Tn)(moreParams)
    //     def _1: T1 = this.p1
    //     ...
    //     def _N: TN = this.pN (unless already given as valdef or parameterless defdef)
    //     def copy(p1: T1 = p1..., pN: TN = pN)(moreParams) =
    //       new C[...](p1, ..., pN)(moreParams)
    val (caseClassMeths, enumScaffolding) = {
      def syntheticProperty(name: TermName, tpt: Tree, rhs: Tree) =
        DefDef(name, Nil, tpt, rhs).withMods(synthetic)

      def productElemMeths =
        if caseClassInScala2Library then Nil
        else
          val caseParams = derivedVparamss.head.toArray
          val selectorNamesInBody = normalizedBody.collect {
            case vdef: ValDef if vdef.name.isSelectorName =>
              vdef.name
            case ddef: DefDef if ddef.name.isSelectorName && ddef.paramss.isEmpty =>
              ddef.name
          }
          for i <- List.range(0, arity)
              selName = nme.selectorName(i)
              if (selName ne caseParams(i).name) && !selectorNamesInBody.contains(selName)
          yield syntheticProperty(selName, caseParams(i).tpt,
            Select(This(EmptyTypeIdent), caseParams(i).name))

      def enumCaseMeths =
        if isEnumCase then
          val (ordinal, scaffolding) = nextOrdinal(className, CaseKind.Class, definesEnumLookupMethods(cdef))
          (ordinalMethLit(ordinal) :: Nil, scaffolding)
        else (Nil, Nil)
      def copyMeths = {
        val hasRepeatedParam = constrVparamss.nestedExists {
          case ValDef(_, tpt, _) => isRepeated(tpt)
        }
        if (mods.is(Abstract) || hasRepeatedParam) Nil  // cannot have default arguments for repeated parameters, hence copy method is not issued
        else {
          val copyFirstParams = derivedVparamss.head.map(vparam =>
            cpy.ValDef(vparam)(rhs = refOfDef(vparam)))
          val copyRestParamss = derivedVparamss.tail.nestedMap(vparam =>
            cpy.ValDef(vparam)(rhs = EmptyTree))
          var flags = Synthetic | constr1.mods.flags & copiedAccessFlags
          if Feature.shouldBehaveAsScala2 then flags &~= Private
          DefDef(
            nme.copy,
            joinParams(derivedTparams, copyFirstParams :: copyRestParamss),
            TypeTree(),
            creatorExpr
          ).withMods(Modifiers(flags, constr1.mods.privateWithin)) :: Nil
        }
      }

      if isCaseClass then
        val (enumMeths, enumScaffolding) = enumCaseMeths
        (copyMeths ::: enumMeths ::: productElemMeths, enumScaffolding)
      else (Nil, Nil)
    }

    var parents1 = parents
    if (isEnumCase && parents.isEmpty)
      parents1 = enumClassTypeRef :: Nil
    if (isNonEnumCase)
      parents1 = parents1 :+ scalaDot(str.Product.toTypeName) :+ scalaDot(nme.Serializable.toTypeName)
    if (isEnum)
      parents1 = parents1 :+ ref(defn.EnumClass)

    // derived type classes of non-module classes go to their companions
    val (clsDerived, companionDerived) =
      if (mods.is(Module)) (implDerived, Nil) else (Nil, implDerived)

    // The thicket which is the desugared version of the companion object
    //     synthetic object C extends parentTpt derives class-derived { defs }
    def companionDefs(parentTpt: Tree, defs: List[Tree]) = {
      val mdefs = moduleDef(
        ModuleDef(
          className.toTermName, Template(emptyConstructor, parentTpt :: Nil, companionDerived, EmptyValDef, defs))
            .withMods(companionMods | Synthetic))
        .withSpan(cdef.span).toList
      if (companionDerived.nonEmpty)
        for (case modClsDef @ TypeDef(_, _) <- mdefs)
          modClsDef.putAttachment(DerivingCompanion, impl.srcPos.startPos)
      mdefs
    }

    val companionMembers = defaultGetters ::: enumCases

    // The companion object definitions, if a companion is needed, Nil otherwise.
    // companion definitions include:
    // 1. If class is a case class case class C[Ts](p1: T1, ..., pN: TN)(moreParams):
    //     def apply[Ts](p1: T1, ..., pN: TN)(moreParams) = new C[Ts](p1, ..., pN)(moreParams)  (unless C is abstract)
    //     def unapply[Ts]($1: C[Ts]) = $1        // if not repeated
    //     def unapplySeq[Ts]($1: C[Ts]) = $1     // if repeated
    // 2. The default getters of the constructor
    // The parent of the companion object of a non-parameterized case class
    //     (T11, ..., T1N) => ... => (TM1, ..., TMN) => C
    // For all other classes, the parent is AnyRef.
    val companions =
      if (isCaseClass) {
        val applyMeths =
          if (mods.is(Abstract)) Nil
          else {
            val appMods =
              var flags = Synthetic | constr1.mods.flags & copiedAccessFlags
              if Feature.shouldBehaveAsScala2 then flags &~= Private
              Modifiers(flags).withPrivateWithin(constr1.mods.privateWithin)
            val appParamss =
              derivedVparamss.nestedZipWithConserve(constrVparamss)((ap, cp) =>
                ap.withMods(ap.mods | (cp.mods.flags & HasDefault)))
            DefDef(nme.apply, joinParams(derivedTparams, appParamss), TypeTree(), creatorExpr)
              .withMods(appMods) :: Nil
          }
        val unapplyMeth = {
          def scala2LibCompatUnapplyRhs(unapplyParamName: Name) =
            assert(arity <= Definitions.MaxTupleArity, "Unexpected case class with tuple larger than 22: "+ cdef.show)
            derivedVparamss.head match
              case vparam :: Nil =>
                Apply(scalaDot(nme.Option), Select(Ident(unapplyParamName), vparam.name))
              case vparams =>
                val tupleApply = Select(Ident(nme.scala), s"Tuple$arity".toTermName)
                val members = vparams.map(vparam => Select(Ident(unapplyParamName), vparam.name))
                Apply(scalaDot(nme.Option), Apply(tupleApply, members))

          val hasRepeatedParam = constrVparamss.head.exists {
            case ValDef(_, tpt, _) => isRepeated(tpt)
          }
          val methName = if (hasRepeatedParam) nme.unapplySeq else nme.unapply
          val unapplyParam = makeSyntheticParameter(tpt = classTypeRef)
          val unapplyRHS =
            if (arity == 0) Literal(Constant(true))
            else if caseClassInScala2Library then scala2LibCompatUnapplyRhs(unapplyParam.name)
            else Ident(unapplyParam.name)
          val unapplyResTp = if (arity == 0) Literal(Constant(true)) else TypeTree()

          DefDef(
            methName,
            joinParams(derivedTparams, (unapplyParam :: Nil) :: Nil),
            unapplyResTp,
            unapplyRHS
          ).withMods(synthetic)
        }
        val toStringMeth =
          DefDef(nme.toString_, Nil, TypeTree(), Literal(Constant(className.toString))).withMods(Modifiers(Override | Synthetic))

        companionDefs(anyRef, applyMeths ::: unapplyMeth :: toStringMeth :: companionMembers)
      }
      else if (companionMembers.nonEmpty || companionDerived.nonEmpty || isEnum)
        companionDefs(anyRef, companionMembers)
      else if isValueClass && !isObject then
        companionDefs(anyRef, Nil)
      else Nil

    enumCompanionRef match {
      case ref: TermRefTree => // have the enum import watch the companion object
        val (modVal: ValDef) :: _ = companions: @unchecked
        ref.watching(modVal)
      case _ =>
    }

    // For an implicit class C[Ts](p11: T11, ..., p1N: T1N) ... (pM1: TM1, .., pMN: TMN), the method
    //     synthetic implicit C[Ts](p11: T11, ..., p1N: T1N) ... (pM1: TM1, ..., pMN: TMN): C[Ts] =
    //       new C[Ts](p11, ..., p1N) ... (pM1, ..., pMN) =
    val implicitWrappers =
      if (!mods.isOneOf(GivenOrImplicit))
        Nil
      else if (ctx.owner.is(Package)) {
        report.error(TopLevelImplicitClass(cdef), cdef.srcPos)
        Nil
      }
      else if (mods.is(Trait)) {
        report.error(TypesAndTraitsCantBeImplicit(), cdef.srcPos)
        Nil
      }
      else if (isCaseClass) {
        report.error(ImplicitCaseClass(cdef), cdef.srcPos)
        Nil
      }
      else if (arity != 1 && !mods.is(Given)) {
        report.error(ImplicitClassPrimaryConstructorArity(), cdef.srcPos)
        Nil
      }
      else {
        val defParamss = constrVparamss match
          case Nil :: paramss =>
            paramss // drop leading () that got inserted by class
                    // TODO: drop this once we do not silently insert empty class parameters anymore
          case paramss => paramss
        val finalFlag = if Feature.shouldBehaveAsScala2 then EmptyFlags else Final
        // implicit wrapper is typechecked in same scope as constructor, so
        // we can reuse the constructor parameters; no derived params are needed.
        DefDef(
          className.toTermName, joinParams(constrTparams, defParamss), classTypeRef, creatorExpr
        ) .withMods(companionMods | mods.flags.toTermFlags & (GivenOrImplicit | Inline) | finalFlag)
          .withSpan(cdef.span) :: Nil
      }

    val self1 = {
      val selfType = if (self.tpt.isEmpty) classTypeRef else self.tpt
      if (self.isEmpty) self
      else cpy.ValDef(self)(tpt = selfType).withMods(self.mods | SelfName)
    }

    val cdef1 = addEnumFlags {
      val tparamAccessors = {
        val impliedTparamsIt = impliedTparams.iterator
        derivedTparams.map(_.withMods(impliedTparamsIt.next().mods))
      }
      val caseAccessor = if (isCaseClass) CaseAccessor else EmptyFlags
      val vparamAccessors = {
        val originalVparamsIt = originalVparamss.iterator.flatten
        derivedVparamss match {
          case first :: rest =>
            first.map(_.withMods(originalVparamsIt.next().mods | caseAccessor)) ++
            rest.flatten.map(_.withMods(originalVparamsIt.next().mods))
          case _ =>
            Nil
        }
      }
      if mods.isAllOf(Given | Inline | Transparent) then
        report.error("inline given instances cannot be trasparent", cdef)
      var classMods = if mods.is(Given) then mods &~ (Inline | Transparent) | Synthetic else mods
      val newBody = tparamAccessors ::: vparamAccessors ::: normalizedBody ::: caseClassMeths
      if newBody.collect { case d: ValOrDefDef => d }.exists(_.mods.is(Tracked)) then
        classMods |= Dependent

      cpy.TypeDef(cdef: TypeDef)(
        name = className,
        rhs = cpy.Template(impl)(constr, parents1, clsDerived, self1,
          newBody)
      ).withMods(addImplUse(classMods, 0))
    }

    // install the watch on classTycon
    classTycon match {
      case tycon: DerivedTypeTree => tycon.watching(cdef1)
      case _ =>
    }

    flatTree(cdef1 :: companions ::: implicitWrappers ::: enumScaffolding)
  }.showing(i"desugared: $cdef --> $result", Printers.desugar)

  /** Expand
   *
   *    package object name { body }
   *
   *  to:
   *
   *    package name {
   *      object `package` { body }
   *    }
   */
  def packageModuleDef(mdef: ModuleDef)(using Context): Tree =
    val impl = mdef.impl
    val mods = mdef.mods
    val moduleName = normalizeName(mdef, impl).asTermName
    if mods.is(Package) then
      checkPackageName(mdef)
      PackageDef(Ident(moduleName),
        cpy.ModuleDef(mdef)(nme.PACKAGE, impl).withMods(mods &~ Package) :: Nil)
    else
      mdef

  /** Expand
   *
   *    object name extends parents { self => body }
   *
   *  to:
   *
   *    <module> val name: name$ = New(name$)
   *    <module> final class name$ extends parents { self: name.type => body }
   */
  def moduleDef(mdef: ModuleDef)(using Context): Tree = {
    val impl = mdef.impl
    val mods = mdef.mods
    val moduleName = normalizeName(mdef, impl).asTermName
    def isEnumCase = mods.isEnumCase
    Checking.checkWellFormedModule(mdef)

    if (mods.is(Package))
      packageModuleDef(mdef)
    else if (isEnumCase) {
      typeParamIsReferenced(enumClass.typeParams, Nil, Nil, impl.parents)
        // used to check there are no illegal references to enum's type parameters in parents
      expandEnumModule(moduleName, impl, mods, definesEnumLookupMethods(mdef), mdef.span)
    }
    else {
      val clsName = moduleName.moduleClassName
      val clsRef = Ident(clsName)
      val modul = ValDef(moduleName, clsRef, New(clsRef, Nil))
        .withMods(mods.toTermFlags & RetainedModuleValFlags | ModuleValCreationFlags)
        .withSpan(mdef.span.startPos)
      val ValDef(selfName, selfTpt, _) = impl.self
      val selfMods = impl.self.mods
      if (!selfTpt.isEmpty) report.error(ObjectMayNotHaveSelfType(mdef), impl.self.srcPos)
      val clsSelf = ValDef(selfName, SingletonTypeTree(Ident(moduleName)), impl.self.rhs)
        .withMods(selfMods)
        .withSpan(impl.self.span.orElse(impl.span.startPos))
      val clsTmpl = cpy.Template(impl)(self = clsSelf, body = impl.body)
      val cls = TypeDef(clsName, clsTmpl)
        .withMods(mods.toTypeFlags & RetainedModuleClassFlags | ModuleClassCreationFlags)
        .withEndMarker(copyFrom = mdef) // copy over the end marker position to the module class def
      Thicket(modul, classDef(cls).withSpan(mdef.span))
    }
  }

  def extMethod(mdef: DefDef, extParamss: List[ParamClause])(using Context): DefDef =
    cpy.DefDef(mdef)(
      name = normalizeName(mdef, mdef.tpt).asTermName,
      paramss =
        if mdef.name.isRightAssocOperatorName then
          val (rightTyParams, paramss) = mdef.paramss.span(isTypeParamClause) // first extract type parameters

          paramss match
            case rightParam :: paramss1 => // `rightParam` must have a single parameter and without `given` flag

              def badRightAssoc(problem: String, pos: SrcPos) =
                report.error(em"right-associative extension method $problem", pos)
                extParamss ++ mdef.paramss

              rightParam match
                case ValDefs(vparam :: Nil) =>
                  if !vparam.mods.is(Given) then
                    // we merge the extension parameters with the method parameters,
                    // swapping the operator arguments:
                    // e.g.
                    //   extension [A](using B)(c: C)(using D)
                    //     def %:[E](f: F)(g: G)(using H): Res = ???
                    // will be encoded as
                    //   def %:[A](using B)[E](f: F)(c: C)(using D)(g: G)(using H): Res = ???
                    //
                    // If you change the names of the clauses below, also change them in right-associative-extension-methods.md
                    val (leftTyParamsAndLeadingUsing, leftParamAndTrailingUsing) = extParamss.span(isUsingOrTypeParamClause)

                    val names = (for ps <- mdef.paramss; p <- ps yield p.name).toSet[Name]

                    val tt = new untpd.UntypedTreeTraverser:
                      def traverse(tree: Tree)(using Context): Unit = tree match
                        case tree: Ident if names.contains(tree.name) =>
                          badRightAssoc(s"cannot have a forward reference to ${tree.name}", tree.srcPos)
                        case _ => traverseChildren(tree)

                    for ts <- leftParamAndTrailingUsing; t <- ts do
                      tt.traverse(t)

                    leftTyParamsAndLeadingUsing ::: rightTyParams ::: rightParam :: leftParamAndTrailingUsing ::: paramss1
                  else
                    badRightAssoc("cannot start with using clause", mdef.srcPos)
                case _ =>
                  badRightAssoc("must start with a single parameter", mdef.srcPos)
            case _ =>
              // no value parameters, so not an infix operator.
              extParamss ++ mdef.paramss
        else
          extParamss ++ mdef.paramss
    ).withMods(mdef.mods | ExtensionMethod)

  /** Transform extension construct to list of extension methods */
  def extMethods(ext: ExtMethods)(using Context): Tree = flatTree {
    ext.methods map {
      case exp: Export => exp
      case mdef: DefDef => defDef(extMethod(mdef, ext.paramss))
    }
  }
  /** Transforms
   *
   *    <mods> type t >: Low <: Hi
   *  to
   *
   *    @patternType <mods> type $T >: Low <: Hi
   *
   *  if the type has a pattern variable name
   */
  def quotedPatternTypeDef(tree: TypeDef)(using Context): TypeDef = {
    assert(ctx.mode.isQuotedPattern)
    if tree.name.isVarPattern && !tree.isBackquoted then
      val patternTypeAnnot = New(ref(defn.QuotedRuntimePatterns_patternTypeAnnot.typeRef)).withSpan(tree.span)
      val mods = tree.mods.withAddedAnnotation(patternTypeAnnot)
      tree.withMods(mods)
    else if tree.name.startsWith("$") && !tree.isBackquoted then
      report.error(
        """Quoted pattern variable names starting with $ are not supported anymore.
          |Use lower cases type pattern name instead.
          |""".stripMargin,
        tree.srcPos)
      tree
    else tree
  }

  def checkSimplePackageName(name: Name, errSpan: Span, source: SourceFile, isPackageObject: Boolean)(using Context) =
    if !ctx.isAfterTyper then
      name match
      case name: SimpleName if (isPackageObject || !errSpan.isSynthetic) && name.exists(Chars.willBeEncoded) =>
        report.warning(EncodedPackageName(name), source.atSpan(errSpan))
      case _ =>

  def checkPackageName(mdef: ModuleDef | PackageDef)(using Context): Unit =
    def check(name: Name, errSpan: Span) = checkSimplePackageName(name, errSpan, mdef.source, isPackageObject = false)
    mdef match
    case pdef: PackageDef =>
      def loop(part: RefTree): Unit = part match
        case part @ Ident(name) => check(name, part.span)
        case part @ Select(qual: RefTree, name) =>
          check(name, part.nameSpan)
          loop(qual)
        case _ =>
      loop(pdef.pid)
    case mdef: ModuleDef if mdef.mods.is(Package) =>
      check(mdef.name, mdef.nameSpan)
    case _ =>

  /** The normalized name of `mdef`. This means
   *   1. Check that the name does not redefine a Scala core class.
   *      If it does redefine, issue an error and return a mangled name instead
   *      of the original one.
   *   2. If the name is missing (this can be the case for instance definitions),
   *      invent one instead.
   */
  def normalizeName(mdef: MemberDef, impl: Tree)(using Context): Name = {
    var name = mdef.name
    if (name.isEmpty) name = name.likeSpaced(inventGivenName(impl))
    def errPos = mdef.source.atSpan(mdef.nameSpan)
    if (ctx.owner == defn.ScalaPackageClass && defn.reservedScalaClassNames.contains(name.toTypeName)) {
      val kind = if (name.isTypeName) "class" else "object"
      report.error(IllegalRedefinitionOfStandardKind(kind, name), errPos)
      name = name.errorName
    }
    name
  }

  /** Strip parens and empty blocks around the body of `tree`. */
  def normalizePolyFunction(tree: PolyFunction)(using Context): PolyFunction =
    def stripped(body: Tree): Tree = body match
      case Parens(body1) =>
        stripped(body1)
      case Block(Nil, body1) =>
        stripped(body1)
      case _ => body
    cpy.PolyFunction(tree)(tree.targs, stripped(tree.body)).asInstanceOf[PolyFunction]

  /** Desugar [T_1, ..., T_M] => (P_1, ..., P_N) => R
   *  Into    scala.PolyFunction { def apply[T_1, ..., T_M](x$1: P_1, ..., x$N: P_N): R }
   */
  def makePolyFunctionType(tree: PolyFunction)(using Context): RefinedTypeTree = (tree: @unchecked) match
    case PolyFunction(tparams: List[untpd.TypeDef] @unchecked, fun @ untpd.Function(vparamTypes, res)) =>
      val paramFlags = fun match
        case fun: FunctionWithMods =>
          // TODO: make use of this in the desugaring when pureFuns is enabled.
          // val isImpure = funFlags.is(Impure)

          // Function flags to be propagated to each parameter in the desugared method type.
          val givenFlag = fun.mods.flags.toTermFlags & Given
          fun.erasedParams.map(isErased => if isErased then givenFlag | Erased else givenFlag)
        case _ =>
          vparamTypes.map(_ => EmptyFlags)

      val vparams = vparamTypes.lazyZip(paramFlags).zipWithIndex.map {
        case ((p: ValDef, paramFlags), n) => p.withAddedFlags(paramFlags)
        case ((p, paramFlags), n) => makeSyntheticParameter(n + 1, p).withAddedFlags(paramFlags)
      }.toList

      RefinedTypeTree(ref(defn.PolyFunctionType), List(
        DefDef(nme.apply, tparams :: vparams :: Nil, res, EmptyTree)
          .withFlags(Synthetic)
          .withAttachment(PolyFunctionApply, ())
      )).withSpan(tree.span)
  end makePolyFunctionType

  /** Invent a name for an anonymous given of type or template `impl`. */
  def inventGivenName(impl: Tree)(using Context): SimpleName =
    val str = impl match
      case impl: Template =>
        if impl.parents.isEmpty then
          report.error(AnonymousInstanceCannotBeEmpty(impl), impl.srcPos)
          nme.ERROR.toString
        else
          impl.parents.map(inventTypeName(_)).mkString("given_", "_", "")
      case impl: Tree =>
        "given_" ++ inventTypeName(impl)
    str.toTermName.asSimpleName

  /** Extract a synthesized given name from a type tree. This is used for
   *  both anonymous givens and deferred givens.
   *  @param followArgs   if true include argument types in the name
   */
  private class NameExtractor(followArgs: Boolean) extends UntypedTreeAccumulator[String] {
    private def extractArgs(args: List[Tree])(using Context): String =
      args.map(argNameExtractor.apply("", _)).mkString("_")
    override def apply(x: String, tree: Tree)(using Context): String =
      if (x.isEmpty)
        tree match {
          case Select(pre, nme.CONSTRUCTOR) => foldOver(x, pre)
          case tree: RefTree =>
            if tree.name.isTypeName then tree.name.toString
            else s"${tree.name}_type"
          case tree: TypeDef => tree.name.toString
          case tree: AppliedTypeTree if followArgs && tree.args.nonEmpty =>
            s"${apply(x, tree.tpt)}_${extractArgs(tree.args)}"
          case ContextBoundTypeTree(tycon, paramName, _) =>
            s"${apply(x, tycon)}_$paramName"
          case InfixOp(left, op, right) =>
            if followArgs then s"${op.name}_${extractArgs(List(left, right))}"
            else op.name.toString
          case tree: LambdaTypeTree =>
            apply(x, tree.body)
          case tree: Tuple =>
            extractArgs(tree.trees)
          case tree: Function if tree.args.nonEmpty =>
            if followArgs then s"${extractArgs(tree.args)}_to_${apply("", tree.body)}"
            else "Function"
          case _ => foldOver(x, tree)
        }
      else x
  }
  private val typeNameExtractor = NameExtractor(followArgs = true)
  private val argNameExtractor = NameExtractor(followArgs = false)

  private def inventTypeName(tree: Tree)(using Context): String = typeNameExtractor("", tree)

  /**This will check if this def tree is marked to define enum lookup methods,
   * this is not recommended to call more than once per tree
   */
  private def definesEnumLookupMethods(ddef: DefTree): Boolean =
    ddef.removeAttachment(DefinesEnumLookupMethods).isDefined

  /**     val p1, ..., pN: T = E
   *  ==>
   *      makePatDef[[val p1: T1 = E]]; ...; makePatDef[[val pN: TN = E]]
   *
   *      case e1, ..., eN
   *  ==>
   *      expandSimpleEnumCase([case e1]); ...; expandSimpleEnumCase([case eN])
   */
  def patDef(pdef: PatDef)(using Context): Tree = flatTree {
    val PatDef(mods, pats, tpt, rhs) = pdef
    if mods.isEnumCase then
      def expand(id: Ident, definesLookups: Boolean) =
        expandSimpleEnumCase(id.name.asTermName, mods, definesLookups,
            Span(id.span.start, id.span.end, id.span.start))

      val ids = pats.asInstanceOf[List[Ident]]
      if definesEnumLookupMethods(pdef) then
        ids.init.map(expand(_, false)) ::: expand(ids.last, true) :: Nil
      else
        ids.map(expand(_, false))
    else {
      val pats1 = if (tpt.isEmpty) pats else pats map (Typed(_, tpt))
      pats1 map (makePatDef(pdef, mods, _, rhs))
    }
  }

  /** The selector of a match, which depends of the given `checkMode`.
   *  @param  sel  the original selector
   *  @return if `checkMode` is
   *           - None              :  sel @unchecked
   *           - Exhaustive        :  sel
   *           - IrrefutablePatDef,
   *             IrrefutableGenFrom:  sel with attachment `CheckIrrefutable -> checkMode`
   */
  def makeSelector(sel: Tree, checkMode: MatchCheck)(using Context): Tree =
    import MatchCheck.*
    checkMode match
    case None               => Annotated(sel, New(ref(defn.UncheckedAnnot.typeRef)))
    case Exhaustive         => sel
    case IrrefutablePatDef
       | IrrefutableGenFrom => sel.withAttachment(CheckIrrefutable, checkMode)
      // TODO: use `pushAttachment` and investigate duplicate attachment

  case class TuplePatternInfo(arity: Int, varNum: Int, wildcardNum: Int)
  object TuplePatternInfo:
    def apply(pat: Tree)(using Context): TuplePatternInfo = pat match
      case Tuple(pats) =>
        var arity = 0
        var varNum = 0
        var wildcardNum = 0
        pats.foreach: p =>
          arity += 1
          p match
            case id: Ident if !isBackquoted(id) =>
              if id.name.isVarPattern then
                varNum += 1
                if id.name == nme.WILDCARD then
                  wildcardNum += 1
            case _ =>
        TuplePatternInfo(arity, varNum, wildcardNum)
      case _ =>
        TuplePatternInfo(-1, -1, -1)
  end TuplePatternInfo

  /** If `pat` is a variable pattern,
   *
   *    val/var/lazy val p = e
   *
   *  Otherwise, in case there is exactly one variable x_1 in pattern
   *   val/var/lazy val p = e  ==>  val/var/lazy val x_1 = (e: @unchecked) match (case p => (x_1))
   *
   *   in case there are zero or more than one variables in pattern
   *   val/var/lazy p = e  ==>  private[this] synthetic [lazy] val t$ = (e: @unchecked) match (case p => (x_1, ..., x_N))
   *                   val/var/def x_1 = t$._1
   *                   ...
   *                   val/var/def x_N = t$._N
   *  If the original pattern variable carries a type annotation, so does the corresponding
   *  ValDef or DefDef.
   */
  def makePatDef(original: Tree, mods: Modifiers, pat: Tree, rhs: Tree)(using Context): Tree = pat match {
    case IdPattern(id, tpt) =>
      val id1 =
        if id.name == nme.WILDCARD
        then cpy.Ident(id)(WildcardParamName.fresh())
        else id
      derivedValDef(original, id1, tpt, rhs, mods)
    case _ =>

      def filterWildcardGivenBinding(givenPat: Bind): Boolean =
        givenPat.name != nme.WILDCARD

      def errorOnGivenBinding(bind: Bind)(using Context): Boolean =
        report.error(
          em"""${hl("given")} patterns are not allowed in a ${hl("val")} definition,
              |please bind to an identifier and use an alias given.""", bind)
        false

      val tuplePatternInfo = TuplePatternInfo(pat)

      // When desugaring a PatDef in general, we use pattern matching on the rhs
      // and collect the variable values in a tuple, then outside the match,
      // we destructure the tuple to get the individual variables.
      // We can achieve two kinds of tuple optimizations if the pattern is a tuple
      // of simple variables or wildcards:
      // 1. Full optimization:
      //    If the rhs is known to produce a literal tuple of the same arity,
      //    we can directly fetch the values from the tuple.
      //    For example: `val (x, y) = if ... then (1, "a") else (2, "b")` becomes
      //    `val $1$ = if ...; val x = $1$._1; val y = $1$._2`.
      // 2. Partial optimization:
      //    If the rhs can be typed as a tuple and matched with correct arity, we can
      //    return the tuple itself in the case if there are no more than one variable
      //    in the pattern, or return the the value if there is only one variable.

      val fullTupleOptimizable =
        val isMatchingTuple: Tree => Boolean = {
          case Tuple(es) => tuplePatternInfo.varNum == es.length && !hasNamedArg(es)
          case _ => false
        }
        tuplePatternInfo.arity > 0
        && tuplePatternInfo.arity == tuplePatternInfo.varNum
        && forallResults(rhs, isMatchingTuple)

      val partialTupleOptimizable =
        tuplePatternInfo.arity > 0
        && tuplePatternInfo.arity == tuplePatternInfo.varNum
        // We exclude the case where there is only one variable,
        // because it should be handled by `makeTuple` directly.
        && tuplePatternInfo.wildcardNum < tuplePatternInfo.arity - 1

      val inAliasGenerator = original match
        case _: GenAlias => true
        case _ => false

      val vars: List[VarInfo] =
        if fullTupleOptimizable || partialTupleOptimizable then // include `_`
          pat match
            case Tuple(pats) => pats.map { case id: Ident => (id, TypeTree()) }
        else
          getVariables(
            tree = pat,
            shouldAddGiven =
              if inAliasGenerator then
                filterWildcardGivenBinding
              else
                errorOnGivenBinding
          ) // no `_`

      val ids = for ((named, tpt) <- vars) yield Ident(named.name)

      val matchExpr =
        if fullTupleOptimizable then rhs
        else
          val caseDef =
            if partialTupleOptimizable then
              val tmpTuple = UniqueName.fresh()
              // Replace all variables with wildcards in the pattern
              val pat1 = pat match
                case Tuple(pats) =>
                  val wildcardPats = pats.map(p => Ident(nme.WILDCARD).withSpan(p.span))
                  Tuple(wildcardPats).withSpan(pat.span)
              CaseDef(
                Bind(tmpTuple, pat1),
                EmptyTree,
                Ident(tmpTuple).withAttachment(ForArtifact, ())
              )
            else CaseDef(pat, EmptyTree, makeTuple(ids).withAttachment(ForArtifact, ()))
          Match(makeSelector(rhs, MatchCheck.IrrefutablePatDef), caseDef :: Nil)

      vars match {
        case Nil if !mods.is(Lazy) =>
          matchExpr
        case (named, tpt) :: Nil =>
          derivedValDef(original, named, tpt, matchExpr, mods)
        case _ =>
          val tmpName = UniqueName.fresh()
          val patMods =
            mods & Lazy | Synthetic | (if (ctx.owner.isClass) PrivateLocal else EmptyFlags)
          val firstDef =
            ValDef(tmpName, TypeTree(), matchExpr)
              .withSpan(pat.span.union(rhs.span)).withMods(patMods)
          val useSelectors = vars.length <= 22
          def selector(n: Int) =
            if useSelectors then Select(Ident(tmpName), nme.selectorName(n))
            else Apply(Select(Ident(tmpName), nme.apply), Literal(Constant(n)) :: Nil)
          val restDefs =
            for (((named, tpt), n) <- vars.zipWithIndex if named.name != nme.WILDCARD)
            yield
              if mods.is(Lazy) then
                DefDef(named.name.asTermName, Nil, tpt, selector(n))
                  .withMods(mods &~ Lazy)
                  .withSpan(named.span)
                  .withAttachment(PatternVar, ())
              else
                valDef(
                  ValDef(named.name.asTermName, tpt, selector(n))
                    .withMods(mods)
                    .withSpan(named.span)
                    .withAttachment(PatternVar, ())
                )
          flatTree(firstDef :: restDefs)
      }
  }

  /** Expand variable identifier x to x @ _ */
  def patternVar(tree: Tree)(using Context): Bind = {
    val Ident(name) = unsplice(tree): @unchecked
    Bind(name, Ident(nme.WILDCARD)).withSpan(tree.span)
  }

  /** The type of tests that check whether a MemberDef is OK for some flag.
   *  The test succeeds if the partial function is defined and returns true.
   */
  type MemberDefTest = PartialFunction[MemberDef, Boolean]

  val legalOpaque: MemberDefTest = {
    case TypeDef(_, rhs) =>
      def rhsOK(tree: Tree): Boolean = tree match {
        case bounds: TypeBoundsTree => !bounds.alias.isEmpty
        case _: Template | _: MatchTypeTree => false
        case LambdaTypeTree(_, body) => rhsOK(body)
        case _ => true
      }
      rhsOK(rhs)
  }

  val legalTracked: Context ?=> MemberDefTest = {
    case valdef @ ValDef(_, _, _) =>
      val sym = valdef.symbol
      !ctx.owner.exists || ctx.owner.isClass || ctx.owner.is(Case) || ctx.owner.isConstructor || valdef.mods.is(Param) || valdef.mods.is(ParamAccessor)
  }

  def checkOpaqueAlias(tree: MemberDef)(using Context): MemberDef =
    def check(rhs: Tree): MemberDef = rhs match
      case bounds: TypeBoundsTree if bounds.alias.isEmpty =>
        report.error(em"opaque type must have a right-hand side", tree.srcPos)
        tree.withMods(tree.mods.withoutFlags(Opaque))
      case LambdaTypeTree(_, body) => check(body)
      case _ => tree
    if !tree.mods.is(Opaque) then tree
    else tree match
      case TypeDef(_, rhs) => check(rhs)
      case _ => tree

  /** Check that modifiers are legal for the definition `tree`.
   *  Right now, we only check for `opaque`. TODO: Move other modifier checks here.
   */
  def checkModifiers(tree: Tree)(using Context): Tree = tree match {
    case tree: MemberDef =>
      var tested: MemberDef = tree
      def checkApplicable(flag: Flag, test: MemberDefTest): MemberDef =
        if (tested.mods.is(flag) && !test.applyOrElse(tree, (md: MemberDef) => false)) {
          report.error(ModifierNotAllowedForDefinition(flag), tree.srcPos)
           tested.withMods(tested.mods.withoutFlags(flag))
        } else tested
      tested = checkOpaqueAlias(tested)
      tested = checkApplicable(Opaque, legalOpaque)
      tested = checkApplicable(Tracked, legalTracked)
      tested
    case _ =>
      tree
  }

  def defTree(tree: Tree)(using Context): Tree =
    checkModifiers(tree) match {
      case tree: ValDef => valDef(tree)
      case tree: TypeDef =>
        if (tree.isClassDef) classDef(tree)
        else if (ctx.mode.isQuotedPattern) quotedPatternTypeDef(tree)
        else typeDef(tree)
      case tree: DefDef =>
        if (tree.name.isConstructorName) tree // was already handled by enclosing classDef
        else defDef(tree)
      case tree: ModuleDef => moduleDef(tree)
      case tree: PatDef => patDef(tree)
    }

  /**     { stats; <empty > }
   *  ==>
   *      { stats; () }
   */
  def block(tree: Block)(using Context): Block = tree.expr match {
    case EmptyTree =>
      cpy.Block(tree)(tree.stats,
        syntheticUnitLiteral.withSpan(if (tree.stats.isEmpty) tree.span else tree.span.endPos))
    case _ =>
      tree
  }

  /** Translate infix operation expression
    *
    *     l op r     ==>    l.op(r)  if op is left-associative
    *                ==>    r.op(l)  if op is right-associative
    */
  def binop(left: Tree, op: Ident, right: Tree)(using Context): Apply = {
    def assignToNamedArg(arg: Tree) = arg match {
      case Assign(Ident(name), rhs) => cpy.NamedArg(arg)(name, rhs)
      case _ => arg
    }
    def makeOp(fn: Tree, arg: Tree, selectPos: Span) =
      val sel = Select(fn, op.name).withSpan(selectPos)
      if (left.sourcePos.endLine < op.sourcePos.startLine)
        sel.pushAttachment(MultiLineInfix, ())
      arg match
        case Parens(arg) =>
          Apply(sel, assignToNamedArg(arg) :: Nil)
        case Tuple(args) if args.exists(_.isInstanceOf[Assign]) =>
          Apply(sel, args.mapConserve(assignToNamedArg))
        case Tuple(args) =>
          Apply(sel, arg :: Nil).setApplyKind(ApplyKind.InfixTuple)
        case _ =>
          Apply(sel, arg :: Nil)

    val apply = if op.name.isRightAssocOperatorName then
      makeOp(right, left, Span(op.span.start, right.span.end))
    else
      makeOp(left, right, Span(left.span.start, op.span.end, op.span.start))
    apply.pushAttachment(WasTypedInfix, ())
    return apply
  }

  /** Translate throws type `A throws E1 | ... | En` to
   *  $throws[... $throws[A, E1] ... , En].
   */
  def throws(tpt: Tree, op: Ident, excepts: Tree)(using Context): AppliedTypeTree = excepts match
    case Parens(excepts1) =>
      throws(tpt, op, excepts1)
    case InfixOp(l, bar @ Ident(tpnme.raw.BAR), r) =>
      throws(throws(tpt, op, l), bar, r)
    case e =>
      AppliedTypeTree(
        TypeTree(defn.throwsAlias.typeRef).withSpan(op.span), tpt :: excepts :: Nil)

  def checkWellFormedTupleElems(elems: List[Tree])(using Context): List[Tree] =
    val seen = mutable.Set[Name]()
    for case arg @ NamedArg(name, _) <- elems do
      if seen.contains(name) then
        report.error(em"Duplicate tuple element name", arg.srcPos)
      seen += name
      if name.startsWith("_") && name.toString.tail.toIntOption.isDefined then
        report.error(
            em"$name cannot be used as the name of a tuple element because it is a regular tuple selector",
            arg.srcPos)

    elems match
    case elem :: elems1 =>
      val mismatchOpt =
        if elem.isInstanceOf[NamedArg]
        then elems1.find(!_.isInstanceOf[NamedArg])
        else elems1.find(_.isInstanceOf[NamedArg])
      mismatchOpt match
        case Some(misMatch) =>
          report.error(em"Illegal combination of named and unnamed tuple elements", misMatch.srcPos)
          elems.mapConserve(stripNamedArg)
        case None => elems
    case _ => elems
  end checkWellFormedTupleElems

  /** Translate tuple expressions
   *
   *     ()                       ==>   ()
   *     (t)                      ==>   t
   *     (t1, ..., tN)            ==>   TupleN(t1, ..., tN)
   *     (n1 = t1, ..., nN = tN)  ==>   NamedTuple.build[(n1, ..., nN)]()(TupleN(t1, ..., tN))
   */
  def tuple(tree: Tuple, pt: Type)(using Context): Tree =
    // Don't adapt pattern args in Type mode (e.g., a typed pattern such as `case _: (a: Int, b: String)`)
    val modePatternNotType = (ctx.mode & Mode.PatternOrTypeBits) == Mode.Pattern
    val elems =
      val wfte = checkWellFormedTupleElems(tree.trees)
      if modePatternNotType then
        adaptPatternArgs(wfte, pt, tree.srcPos)
      else
        wfte
    val elemValues = elems.mapConserve(stripNamedArg)
    val tup =
      val arity = elems.length
      if arity <= Definitions.MaxTupleArity then
        def tupleTypeRef = defn.TupleType(arity).nn
        val tree1 =
          if ctx.mode is Mode.Type then
            if arity == 0 then
              TypeTree(defn.UnitType)
            else
              AppliedTypeTree(ref(tupleTypeRef), elemValues)
          else
            if arity == 0 then
              unitLiteral
            else
              Apply(ref(tupleTypeRef.classSymbol.companionModule.termRef), elemValues)
        tree1.withSpan(tree.span)
      else
        cpy.Tuple(tree)(elemValues)
    if modePatternNotType || !elems.exists(_.isInstanceOf[NamedArg]) then
      tup
    else
      val names = elems.collect:
        case NamedArg(name, arg) => name
      def namesTuple = withModeBits(ctx.mode &~ Mode.Pattern | Mode.Type):
        tuple(Tuple(
          names.map: name =>
            SingletonTypeTree(Literal(Constant(name.toString))).withSpan(tree.span)),
          WildcardType)
      if ctx.mode.is(Mode.Type) then
        AppliedTypeTree(ref(defn.NamedTupleTypeRef), namesTuple :: tup :: Nil)
      else
        Apply(
          Apply(
            TypeApply(
              Select(ref(defn.NamedTupleModule), nme.build), // NamedTuple.build
              namesTuple :: Nil), // ++ [(names...)]
            Nil), // ++ ()
          tup :: Nil) // .++ ((values...))

  /** When desugaring a list pattern arguments `elems` adapt them and the
   *  expected type `pt` to each other. This means:
   *   - If `elems` are named pattern elements, rearrange them to match `pt`.
   *     This requires all names in `elems` to be also present in `pt`.
   */
  def adaptPatternArgs(elems: List[Tree], pt: Type, pos: SrcPos)(using Context): List[Tree] =

    def reorderedNamedArgs(wildcardSpan: Span): List[untpd.Tree] =
      inline def isCaseClass = pt.classSymbol.is(CaseClass) && !defn.isTupleClass(pt.classSymbol)
      if !isCaseClass && !pt.isNamedTupleType then
        report.error(NamedPatternNotApplicable(pt), pos)
        Nil
      else
        var selNames = pt.namedTupleElementTypes(false).map(_(0))
        if isCaseClass && selNames.isEmpty then
          selNames = pt.classSymbol.caseAccessors.map(_.name.asTermName)
        val nameToIdx = selNames.zipWithIndex.toMap
        val reordered = Array.fill[untpd.Tree](selNames.length):
          untpd.Ident(nme.WILDCARD).withSpan(wildcardSpan)
        for case arg @ NamedArg(name: TermName, _) <- elems do
          nameToIdx.get(name) match
            case Some(idx) =>
              if reordered(idx).isInstanceOf[Ident] then
                reordered(idx) = arg
              else
                report.error(em"Duplicate named pattern", arg.srcPos)
            case _ =>
              report.error(em"No element named `$name` is defined in selector type $pt", arg.srcPos)
        reordered.toList
      end if

    elems match
      case (first @ NamedArg(_, _)) :: _ => reorderedNamedArgs(first.span.startPos)
      case _ => elems
  end adaptPatternArgs

  private def isTopLevelDef(stat: Tree)(using Context): Boolean = stat match
    case _: ValDef | _: PatDef | _: DefDef | _: Export | _: ExtMethods => true
    case stat: ModuleDef =>
      stat.mods.isOneOf(GivenOrImplicit)
    case stat: TypeDef =>
      !stat.isClassDef || stat.mods.isOneOf(GivenOrImplicit)
    case _ =>
      false

  /** Assuming `src` contains top-level definition, returns the name that should
   *  be using for the package object that will wrap them.
   */
  def packageObjectName(src: SourceFile): TermName =
    val fileName = src.file.name
    val sourceName = fileName.take(fileName.lastIndexOf('.'))
    (sourceName ++ str.TOPLEVEL_SUFFIX).toTermName

  /** Group all definitions that can't be at the toplevel in
   *  an object named `<source>$package` where `<source>` is the name of the source file.
   *  Definitions that can't be at the toplevel are:
   *
   *   - all pattern, value and method definitions
   *   - non-class type definitions
   *   - implicit classes and objects
   *   - "companion objects" of wrapped type definitions
   *     (i.e. objects having the same name as a wrapped type)
   */
  def packageDef(pdef: PackageDef)(using Context): PackageDef = {
    checkPackageName(pdef)
    val wrappedTypeNames = pdef.stats.collect {
      case stat: TypeDef if isTopLevelDef(stat) => stat.name
    }
    def inPackageObject(stat: Tree) =
      isTopLevelDef(stat) || {
        stat match
          case stat: ModuleDef =>
            wrappedTypeNames.contains(stat.name.stripModuleClassSuffix.toTypeName)
          case _ =>
            false
      }
    val (nestedStats, topStats) = pdef.stats.partition(inPackageObject)
    if (nestedStats.isEmpty) pdef
    else {
      val name = packageObjectName(ctx.source)
      val grouped =
        ModuleDef(name, Template(emptyConstructor, Nil, Nil, EmptyValDef, nestedStats))
          .withMods(Modifiers(Synthetic))
      cpy.PackageDef(pdef)(pdef.pid, topStats :+ grouped)
    }
  }

  /** Make closure corresponding to function.
   *      [tparams] => params => body
   *  ==>
   *      def $anonfun[tparams](params) = body
   *      Closure($anonfun)
   */
  def makeClosure(tparams: List[TypeDef], vparams: List[ValDef], body: Tree, tpt: Tree | Null = null, span: Span)(using Context): Block =
    val paramss: List[ParamClause] =
      if tparams.isEmpty then vparams :: Nil
      else tparams :: vparams :: Nil
    Block(
      DefDef(nme.ANON_FUN, paramss, if (tpt == null) TypeTree() else tpt, body)
        .withSpan(span)
        .withMods(synthetic | Artifact),
      Closure(Nil, Ident(nme.ANON_FUN), EmptyTree).withSpan(span))

  /** If `nparams` == 1, expand partial function
   *
   *       { cases }
   *  ==>
   *       x$1 => (x$1 @unchecked?) match { cases }
   *
   *  If `nparams` != 1, expand instead to
   *
   *       (x$1, ..., x$n) => (x$0, ..., x${n-1} @unchecked?) match { cases }
   */
  def makeCaseLambda(cases: List[CaseDef], checkMode: MatchCheck, nparams: Int = 1)(using Context): Function = {
    val params = (1 to nparams).toList.map(makeSyntheticParameter(_))
    val selector = makeTuple(params.map(p => Ident(p.name)))
    Function(params, Match(makeSelector(selector, checkMode), cases))
  }

  /** Map n-ary function `(x1: T1, ..., xn: Tn) => body` where n != 1 to unary function as follows:
   *
   *    (x$1: (T1, ..., Tn)) => {
   *      val x1: T1 = x$1._1
   *      ...
   *      val xn: Tn = x$1._n
   *      body
   *    }
   *
   *  or if `isGenericTuple`
   *
   *    (x$1: (T1, ... Tn) => {
   *      val x1: T1 = x$1.apply(0)
   *      ...
   *      val xn: Tn = x$1.apply(n-1)
   *      body
   *    }
   *
   *  If some of the Ti's are absent, omit the : (T1, ..., Tn) type ascription
   *  in the selector.
   */
  def makeTupledFunction(params: List[ValDef], body: Tree, isGenericTuple: Boolean)(using Context): Tree = {
    val param = makeSyntheticParameter(
      tpt =
        if params.exists(_.tpt.isEmpty) then TypeTree()
        else Tuple(params.map(_.tpt)),
      flags =
        if params.nonEmpty && params.head.mods.is(Given) then SyntheticTermParam | Given
        else SyntheticTermParam)
    def selector(n: Int) =
      if (isGenericTuple) Apply(Select(refOfDef(param), nme.apply), Literal(Constant(n)))
      else Select(refOfDef(param), nme.selectorName(n))
    val vdefs =
      params.zipWithIndex.map {
        case (param, idx) =>
          ValDef(param.name, param.tpt, selector(idx))
            .withSpan(param.span)
            .withAttachment(UntupledParam, ())
      }
    Function(param :: Nil, Block(vdefs, body))
  }

  /** Convert a tuple pattern with given `elems` to a sequence of `ValDefs`,
   *  skipping elements that are not convertible.
   */
  def patternsToParams(elems: List[Tree])(using Context): List[ValDef] =
    def toParam(elem: Tree, tpt: Tree, span: Span): Tree =
      elem match
        case Annotated(elem1, _) => toParam(elem1, tpt, span)
        case Typed(elem1, tpt1) => toParam(elem1, tpt1, span)
        case Ident(id: TermName) => ValDef(id, tpt, EmptyTree).withFlags(Param).withSpan(span)
        case _ => EmptyTree
    elems
      .map: param =>
        toParam(param, TypeTree(), param.span)
      .collect:
        case vd: ValDef => vd

  def makeContextualFunction(formals: List[Tree], paramNamesOrNil: List[TermName], body: Tree, erasedParams: List[Boolean], augmenting: Boolean = false)(using Context): Function =
    val paramNames =
      if paramNamesOrNil.nonEmpty then
        if augmenting then paramNamesOrNil.map(ContextFunctionParamName.fresh(_))
        else paramNamesOrNil
      else List.fill(formals.length)(ContextFunctionParamName.fresh())
    val params = for (tpt, pname) <- formals.zip(paramNames) yield
      ValDef(pname, tpt, EmptyTree).withFlags(Given | Param)
    FunctionWithMods(params, body, Modifiers(Given), erasedParams)

  private def derivedValDef(original: Tree, named: NameTree, tpt: Tree, rhs: Tree, mods: Modifiers)(using Context) = {
    val vdef = ValDef(named.name.asTermName, tpt, rhs)
      .withMods(mods)
      .withSpan(original.span.withPoint(named.span.start))
      .withAttachment(PatternVar, ())
    val mayNeedSetter = valDef(vdef)
    mayNeedSetter
  }

  @unused
  private def derivedDefDef(original: Tree, named: NameTree, tpt: Tree, rhs: Tree, mods: Modifiers)(implicit src: SourceFile) =
    DefDef(named.name.asTermName, Nil, tpt, rhs)
      .withMods(mods)
      .withSpan(original.span.withPoint(named.span.start))

  /** Main desugaring method */
  def apply(tree: Tree, pt: Type = NoType)(using Context): Tree = {

    /** Create tree for for-comprehension `<for (enums) do body>` or
     *   `<for (enums) yield body>` where mapName and flatMapName are chosen
     *  corresponding to whether this is a for-do or a for-yield.
     *  If betterFors are enabled, the creation performs the following rewrite rules:
     *
     *  1. if betterFors is enabled:
     *
     *    for () do E  ==>  E
     *      or
     *    for () yield E  ==>  E
     *
     *    (Where empty for-comprehensions are excluded by the parser)
     *
     *  2.
     *
     *    for (P <- G) do E   ==>   G.foreach (P => E)
     *
     *    Here and in the following (P => E) is interpreted as the function (P => E)
     *    if P is a variable pattern and as the partial function { case P => E } otherwise.
     *
     *  3.
     *
     *    for (P <- G) yield E  ==>  G.map (P => E)
     *
     *  4.
     *
     *    for (P_1 <- G_1; P_2 <- G_2; ...) ...
     *      ==>
     *    G_1.flatMap (P_1 => for (P_2 <- G_2; ...) ...)
     *
     *  5.
     *
     *    for (P <- G; if E; ...) ...
     *      ==>
     *    for (P <- G.withFilter (P => E); ...) ...
     *
     *  6. For any N, if betterFors is enabled:
     *
     *    for (P <- G; P_1 = E_1; ... P_N = E_N; P1 <- G1; ...) ...
     *      ==>
     *    G.flatMap (P => for (P_1 = E_1; ... P_N = E_N; ...))
     *
     *  7. For any N, if betterFors is enabled:
     *
     *    for (P <- G; P_1 = E_1; ... P_N = E_N) ...
     *      ==>
     *    G.map (P => for (P_1 = E_1; ... P_N = E_N) ...)
     *
     *  8. For any N:
     *
     *    for (P <- G; P_1 = E_1; ... P_N = E_N; ...)
     *      ==>
     *    for (TupleN(P, P_1, ... P_N) <-
     *      for (x @ P <- G) yield {
     *        val x_1 @ P_1 = E_2
     *        ...
     *        val x_N @ P_N = E_N
     *        TupleN(x, x_1, ..., x_N)
     *      }; if E; ...)
     *
     *    If any of the P_i are variable patterns, the corresponding `x_i @ P_i` is not generated
     *    and the variable constituting P_i is used instead of x_i
     *
     *  9. For any N, if betterFors is enabled:
     *
     *    for (P_1 = E_1; ... P_N = E_N; ...)
     *      ==>
     *    {
     *      val x_N @ P_N = E_N
     *      for (...)
     *    }
     *
     *  @param mapName      The name to be used for maps (either map or foreach)
     *  @param flatMapName  The name to be used for flatMaps (either flatMap or foreach)
     *  @param enums        The enumerators in the for expression
     *  @param body         The body of the for expression
     */
    def makeFor(mapName: TermName, flatMapName: TermName, enums: List[Tree], body: Tree): Tree = trace(i"make for ${ForYield(enums, body)}", show = true) {

      /** Let `pat` be `gen`'s pattern. Make a function value `pat => body`.
       *  If `pat` is a var pattern `id: T` then this gives `(id: T) => body`.
       *  Otherwise this gives `{ case pat => body }`, where `pat` is checked to be
       *  irrefutable if `gen`'s checkMode is GenCheckMode.Check.
       */
      def makeLambda(gen: GenFrom, body: Tree): Tree = gen.pat match {
        case IdPattern(named, tpt) if gen.checkMode != GenCheckMode.FilterAlways =>
          Function(derivedValDef(gen.pat, named, tpt, EmptyTree, Modifiers(Param)) :: Nil, body)
        case _ =>
          val matchCheckMode =
            if (gen.checkMode == GenCheckMode.Check || gen.checkMode == GenCheckMode.CheckAndFilter) MatchCheck.IrrefutableGenFrom
            else MatchCheck.None
          val pat = gen.pat.match
            case Tuple(pats) if pats.length > Definitions.MaxImplementedFunctionArity =>
              /* The pattern case is a tupleXXL, because we have bound > 21 variables in the comprehension.
               * In this case, we need to mark all the typed patterns as @unchecked, or get loads of warnings.
               * Cf. warn test i23164.scala */
              Tuple:
                pats.map:
                  case t @ Bind(name, tp @ Typed(id, tpt)) =>
                    val annotated = Annotated(tpt, New(ref(defn.UncheckedAnnot.typeRef)))
                    cpy.Bind(t)(name, cpy.Typed(tp)(id, annotated)).withMods(t.mods)
                  case t => t
            case _ => gen.pat
          makeCaseLambda(CaseDef(pat, EmptyTree, body) :: Nil, matchCheckMode)
      }

      def hasGivenBind(pat: Tree): Boolean = pat.existsSubTree {
        case pat @ Bind(_, pat1) => pat.mods.is(Given)
        case _ => false
      }

      /** Does this pattern define any given bindings */
      def isNestedGivenPattern(pat: Tree): Boolean = pat match
        case pat @ Bind(_, pat1) => hasGivenBind(pat1)
        case _ => hasGivenBind(pat)

      /** If `pat` is not an Identifier, a Typed(Ident, _), or a Bind, wrap
       *  it in a Bind with a fresh name. Return the transformed pattern, and the identifier
       *  that refers to the bound variable for the pattern. Wildcard Binds are
       *  also replaced by Binds with fresh names.
       */
      def makeIdPat(pat: Tree): (Tree, Ident) = pat match
        case pat @ Bind(nme.WILDCARD, body) =>
          val name =
            body match
            case Typed(Ident(nme.WILDCARD), tpt) if pat.mods.is(Given) => inventGivenName(tpt)
            case _ => UniqueName.fresh()
          (cpy.Bind(pat)(name, body).withMods(pat.mods), Ident(name))
        case Bind(name, _) => (pat, Ident(name))
        case id: Ident if isVarPattern(id) && id.name != nme.WILDCARD => (id, id)
        case Typed(id: Ident, _) if isVarPattern(id) && id.name != nme.WILDCARD => (pat, id)
        case _ =>
          val name = UniqueName.fresh()
          (Bind(name, pat), Ident(name))

      /** Make a pattern filter:
       *    rhs.withFilter { case pat => true case _ => false }
       *
       *  On handling irrefutable patterns:
       *  The idea is to wait until the pattern matcher sees a call
       *
       *      xs withFilter { cases }
       *
       *  where cases can be proven to be refutable i.e. cases would be
       *  equivalent to  { case _ => true }
       *
       *  In that case, compile to
       *
       *      xs withFilter alwaysTrue
       *
       *  where `alwaysTrue` is a predefined function value:
       *
       *      val alwaysTrue: Any => Boolean = true
       *
       *  In the libraries operations can take advantage of alwaysTrue to shortcircuit the
       *  withFilter call.
       *
       *  def withFilter(f: Elem => Boolean) =
       *    if (f eq alwaysTrue) this // or rather identity filter monadic applied to this
       *    else real withFilter
       */
      def makePatFilter(rhs: Tree, pat: Tree): Tree = {
        val cases = List(
          CaseDef(pat, EmptyTree, Literal(Constant(true))),
          CaseDef(Ident(nme.WILDCARD), EmptyTree, Literal(Constant(false))))
        Apply(Select(rhs, nme.withFilter), makeCaseLambda(cases, MatchCheck.None))
      }

      /** Is pattern `pat` irrefutable when matched against `rhs`?
       *  We only can do a simple syntactic check here; a more refined check
       *  is done later in the pattern matcher (see discussion in @makePatFilter).
       */
      def isIrrefutable(pat: Tree, rhs: Tree): Boolean = {
        def matchesTuple(pats: List[Tree], rhs: Tree): Boolean = rhs match {
          case Tuple(trees) => (pats corresponds trees)(isIrrefutable)
          case Parens(rhs1) => matchesTuple(pats, rhs1)
          case Block(_, rhs1) => matchesTuple(pats, rhs1)
          case If(_, thenp, elsep) => matchesTuple(pats, thenp) && matchesTuple(pats, elsep)
          case Match(_, cases) => cases forall (matchesTuple(pats, _))
          case CaseDef(_, _, rhs1) => matchesTuple(pats, rhs1)
          case Throw(_) => true
          case _ => false
        }
        pat match {
          case Bind(_, pat1) => isIrrefutable(pat1, rhs)
          case Parens(pat1) => isIrrefutable(pat1, rhs)
          case Tuple(pats) => matchesTuple(pats, rhs)
          case _ => isVarPattern(pat)
        }
      }

      /** Is `pat` of the form `x`, `x T`, or `given T`? when used as the lhs of a generator,
       *  these are all considered irrefutable.
       */
      def isVarBinding(pat: Tree): Boolean = pat match
        case pat @ Bind(_, pat1) if pat.mods.is(Given) => isVarBinding(pat1)
        case IdPattern(_) => true
        case _ => false

      def needsNoFilter(gen: GenFrom): Boolean = gen.checkMode match
        case GenCheckMode.FilterAlways => false  // pattern was prefixed by `case`
        case GenCheckMode.FilterNow | GenCheckMode.CheckAndFilter => isVarBinding(gen.pat) || isIrrefutable(gen.pat, gen.expr)
        case GenCheckMode.Check => true
        case GenCheckMode.Ignore | GenCheckMode.Filtered => true

      /** rhs.name with a pattern filter on rhs unless `pat` is irrefutable when
       *  matched against `rhs`.
       */
      def rhsSelect(gen: GenFrom, name: TermName) = {
        val rhs = if (needsNoFilter(gen)) gen.expr else makePatFilter(gen.expr, gen.pat)
        Select(rhs, name)
      }

      def deepEquals(t1: Tree, t2: Tree): Boolean =
        (unsplice(t1), unsplice(t2)) match
          case (Ident(n1), Ident(n2)) => n1 == n2
          case (Tuple(ts1), Tuple(ts2)) => ts1.corresponds(ts2)(deepEquals)
          case _ => false

      def markTrailingMap(aply: Apply, gen: GenFrom, selectName: TermName): Unit =
        if sourceVersion.enablesBetterFors
          && selectName == mapName
          && gen.checkMode != GenCheckMode.Filtered // results of withFilter have the wrong type
          && (deepEquals(gen.pat, body) || deepEquals(body, Tuple(Nil)))
        then
          aply.putAttachment(TrailingForMap, ())

      enums match {
        case Nil if sourceVersion.enablesBetterFors => body
        case (gen: GenFrom) :: Nil =>
          Apply(rhsSelect(gen, mapName), makeLambda(gen, body))
            .tap(markTrailingMap(_, gen, mapName))
        case (gen: GenFrom) :: (rest @ (GenFrom(_, _, _) :: _)) =>
          val cont = makeFor(mapName, flatMapName, rest, body)
          Apply(rhsSelect(gen, flatMapName), makeLambda(gen, cont))
        case (gen: GenFrom) :: (rest @ GenAlias(_, _) :: _) =>
          val (valeqs, suffix) = rest.span(_.isInstanceOf[GenAlias])
          // possible aliases followed by a generator or end of for, when betterFors.
          // exclude value definitions with a given pattern (given T = x)
          val better = sourceVersion.enablesBetterFors
            && suffix.headOption.forall(_.isInstanceOf[GenFrom])
            && !valeqs.exists(a => isNestedGivenPattern(a.asInstanceOf[GenAlias].pat))
          if better then
            val cont = makeFor(mapName, flatMapName, enums = rest, body)
            val selectName =
              if suffix.exists(_.isInstanceOf[GenFrom]) then flatMapName
              else mapName
            Apply(rhsSelect(gen, selectName), makeLambda(gen, cont))
          else
            val (pats, rhss) = valeqs.map { case GenAlias(pat, rhs) => (pat, rhs) }.unzip
            val (defpat0, id0) = makeIdPat(gen.pat)
            val (defpats, ids) = pats.map(makeIdPat).unzip
            val pdefs = valeqs.lazyZip(defpats).lazyZip(rhss).map: (valeq, defpat, rhs) =>
              val mods = defpat match
                case defTree: DefTree => defTree.mods
                case _ => Modifiers()
              makePatDef(valeq, mods, defpat, rhs)
            val rhs1 =
              val enums = GenFrom(defpat0, gen.expr, gen.checkMode) :: Nil
              val body = Block(pdefs, makeTuple(id0 :: ids).withAttachment(ForArtifact, ()))
              makeFor(nme.map, nme.flatMap, enums, body)
            val allpats = gen.pat :: pats
            val vfrom1 = GenFrom(makeTuple(allpats), rhs1, GenCheckMode.Ignore)
            makeFor(mapName, flatMapName, enums = vfrom1 :: suffix, body)
          end if
        case (gen: GenFrom) :: test :: rest =>
          val genFrom =
            val filtered = Apply(rhsSelect(gen, nme.withFilter), makeLambda(gen, test))
            val mode =
              import GenCheckMode.*
              if sourceVersion.enablesBetterFors then
                if gen.checkMode eq FilterAlways then FilterAlways
                else Filtered
              else Ignore
            GenFrom(gen.pat, filtered, mode)
          makeFor(mapName, flatMapName, genFrom :: rest, body)
        case enums @ GenAlias(_, _) :: _ if sourceVersion.enablesBetterFors =>
          val (valeqs, suffix) = enums.span(_.isInstanceOf[GenAlias])
          val (pats, rhss) = valeqs.map { case GenAlias(pat, rhs) => (pat, rhs) }.unzip
          val (defpats, ids) = pats.map(makeIdPat).unzip
          val pdefs = valeqs.lazyZip(defpats).lazyZip(rhss).map: (valeq, defpat, rhs) =>
            val mods = defpat match
              case defTree: DefTree => defTree.mods
              case _ => Modifiers()
            makePatDef(valeq, mods, defpat, rhs)
          Block(pdefs, makeFor(mapName, flatMapName, enums = suffix, body))
        case _ =>
          EmptyTree //may happen for erroneous input
      }
    }

    // begin desugar

    // Special case for `Parens` desugaring: unlike all the desugarings below,
    // its output is not a new tree but an existing one whose position should
    // be preserved, so we shouldn't call `withPos` on it.
    tree match {
      case Parens(t) =>
        return t
      case _ =>
    }

    val desugared = tree match {
      case SymbolLit(str) =>
        Apply(
          ref(defn.ScalaSymbolClass.companionModule.termRef),
          Literal(Constant(str)) :: Nil)
      case InterpolatedString(id, segments) =>
        val strs = segments map {
          case ts: Thicket => ts.trees.head
          case t => t
        }
        val elems = segments flatMap {
          case ts: Thicket => ts.trees.tail
          case t => Nil
        } map {
          case Block(Nil, EmptyTree) => syntheticUnitLiteral // for s"... ${} ..."
          case Block(Nil, expr) => expr // important for interpolated string as patterns, see i1773.scala
          case t => t
        }
        // This is a deliberate departure from scalac, where StringContext is not rooted (See #4732)
        Apply(Select(Apply(scalaDot(nme.StringContext), strs), id).withSpan(tree.span), elems)
      case PostfixOp(t, op) =>
        if (ctx.mode is Mode.Type) && !isBackquoted(op) && op.name == tpnme.raw.STAR then
          if ctx.isJava then
            AppliedTypeTree(ref(defn.RepeatedParamType), t)
          else
            Annotated(
              AppliedTypeTree(ref(defn.SeqType), t),
              New(ref(defn.RepeatedAnnot.typeRef), Nil :: Nil))
        else
          assert(ctx.mode.isExpr || ctx.reporter.errorsReported || ctx.mode.is(Mode.Interactive), ctx.mode)
          Select(t, op.name)
      case PrefixOp(op, t) =>
        val nspace = if (ctx.mode.is(Mode.Type)) tpnme else nme
        Select(t, nspace.UNARY_PREFIX ++ op.name)
      case ForDo(enums, body) =>
        makeFor(nme.foreach, nme.foreach, enums, body) `orElse` tree
      case ForYield(enums, body) =>
        makeFor(nme.map, nme.flatMap, enums, body) `orElse` tree
      case PatDef(mods, pats, tpt, rhs) =>
        val pats1 = if (tpt.isEmpty) pats else pats map (Typed(_, tpt))
        flatTree(pats1 map (makePatDef(tree, mods, _, rhs)))
      case ext: ExtMethods =>
        Block(List(ext), syntheticUnitLiteral.withSpan(ext.span))
      case f: FunctionWithMods if f.hasErasedParams => makeFunctionWithValDefs(f, pt)
    }
    desugared.withSpan(tree.span)
  }

  /** Turn a fucntion value `handlerFun` into a catch case for a try.
   *  If `handlerFun` is a partial function, translate to
   *
   *    case ex =>
   *      val ev$1 = handlerFun
   *      if ev$1.isDefinedAt(ex) then ev$1.apply(ex) else throw ex
   *
   *  Otherwise translate to
   *
   *     case ex => handlerFun.apply(ex)
   */
  def makeTryCase(handlerFun: tpd.Tree)(using Context): CaseDef =
    val handler = TypedSplice(handlerFun)
    val excId = Ident(nme.DEFAULT_EXCEPTION_NAME)
    val rhs =
      if handlerFun.tpe.widen.isRef(defn.PartialFunctionClass) then
        val tmpName = UniqueName.fresh()
        val tmpId = Ident(tmpName)
        val init = ValDef(tmpName, TypeTree(), handler)
        val test = If(
          Apply(Select(tmpId, nme.isDefinedAt), excId),
          Apply(Select(tmpId, nme.apply), excId),
          Throw(excId))
        Block(init :: Nil, test)
      else
        Apply(Select(handler, nme.apply), excId)
    CaseDef(excId, EmptyTree, rhs)

  /** Create a class definition with the same info as the refined type given by `parent`
   *  and `refinements`.
   *
   *      parent { refinements }
   *  ==>
   *      trait <refinement> extends core { this: self => refinements }
   *
   *  Here, `core` is the (possibly parameterized) class part of `parent`.
   *  If `parent` is the same as `core`, self is empty. Otherwise `self` is `parent`.
   *
   *  Example: Given
   *
   *      class C
   *      type T1 = C { type T <: A }
   *
   *  the refined type
   *
   *      T1 { type T <: B }
   *
   *  is expanded to
   *
   *      trait <refinement> extends C { this: T1 => type T <: A }
   *
   *  The result of this method is used for validity checking, is thrown away afterwards.
   *  @param parent  The type of `parent`
   */
  def refinedTypeToClass(parent: tpd.Tree, refinements: List[Tree])(using Context): TypeDef = {
    def stripToCore(tp: Type): List[Type] = tp match {
      case tp: AppliedType => tp :: Nil
      case tp: TypeRef if tp.symbol.isClass => tp :: Nil     // monomorphic class type
      case tp: TypeProxy => stripToCore(tp.underlying)
      case AndType(tp1, tp2) => stripToCore(tp1) ::: stripToCore(tp2)
      case _ => defn.AnyType :: Nil
    }

    val refinements1 = Trees.flatten:
      refinements.mapConserve {
        case tree: ValDef if tree.mods.is(Mutable) =>
          val getter =
            cpy.DefDef(tree)(name = tree.name, paramss = Nil, tpt = tree.tpt, rhs = tree.rhs)
              .withFlags(tree.mods.flags & (AccessFlags | Synthetic))
          val setterParam = makeSyntheticParameter(tpt = tree.tpt)
          val setter =
            cpy.DefDef(tree)(name = tree.name.setterName, paramss = List(List(setterParam)), tpt = untpd.scalaUnit, rhs = EmptyTree)
              .withFlags(tree.mods.flags & (AccessFlags | Synthetic))
          Thicket(getter, setter)
        case tree => tree
      }

    val parentCores = stripToCore(parent.tpe)
    val untpdParent = TypedSplice(parent)
    val (classParents, self) =
      if (parentCores.length == 1 && (parent.tpe eq parentCores.head)) (untpdParent :: Nil, EmptyValDef)
      else (parentCores map TypeTree, ValDef(nme.WILDCARD, untpdParent, EmptyTree))
    val impl = Template(emptyConstructor, classParents, Nil, self, refinements1)
    TypeDef(tpnme.REFINE_CLASS, impl).withFlags(Trait)
  }

  /** Ensure the given function tree use only ValDefs for parameters.
   *  For example,
   *      FunctionWithMods(List(TypeTree(A), TypeTree(B)), body, mods, erasedParams)
   *  gets converted to
   *      FunctionWithMods(List(ValDef(x$1, A), ValDef(x$2, B)), body, mods, erasedParams)
   */
  def makeFunctionWithValDefs(tree: Function, pt: Type)(using Context): Function = {
    val Function(args, result) = tree
    args match {
      case (_ : ValDef) :: _ => tree // ValDef case can be easily handled
      case _ if !ctx.mode.is(Mode.Type) => tree
      case _ =>
        val applyVParams = args.zipWithIndex.map {
          case (p, n) => makeSyntheticParameter(n + 1, p)
        }
        cpy.Function(tree)(applyVParams, result).asInstanceOf[untpd.Function]
    }
  }

  /** Returns list of all pattern variables, possibly with their types,
   *  without duplicates
   */
  private def getVariables(tree: Tree, shouldAddGiven: Context ?=> Bind => Boolean)(using Context): List[VarInfo] = {
    val buf = ListBuffer.empty[VarInfo]
    def seenName(name: Name) = buf exists (_._1.name == name)
    def add(named: NameTree, t: Tree): Unit =
      if (!seenName(named.name) && named.name.isTermName) buf += ((named, t))
    def collect(tree: Tree): Unit = tree match {
      case tree @ Bind(nme.WILDCARD, tree1) =>
        if tree.mods.is(Given) then
          val Typed(_, tpt) = tree1: @unchecked
          if shouldAddGiven(tree) then
            add(tree, tpt)
        collect(tree1)
      case tree @ Bind(_, Typed(tree1, tpt)) =>
        if !(tree.mods.is(Given) && !shouldAddGiven(tree)) then
          add(tree, tpt)
        collect(tree1)
      case tree @ Bind(_, tree1) =>
        add(tree, TypeTree())
        collect(tree1)
      case Typed(id: Ident, t) if isVarPattern(id) && id.name != nme.WILDCARD && !isWildcardStarArg(tree) =>
        add(id, t)
      case id: Ident if isVarPattern(id) && id.name != nme.WILDCARD =>
        add(id, TypeTree())
      case Apply(_, args) =>
        args foreach collect
      case Typed(expr, _) =>
        collect(expr)
      case NamedArg(_, arg) =>
        collect(arg)
      case SeqLiteral(elems, _) =>
        elems foreach collect
      case Alternative(trees) =>
        for (tree <- trees; (vble, _) <- getVariables(tree, shouldAddGiven))
          report.error(IllegalVariableInPatternAlternative(vble.symbol.name), vble.srcPos)
      case Annotated(arg, _) =>
        collect(arg)
      case InterpolatedString(_, segments) =>
        segments foreach collect
      case InfixOp(left, _, right) =>
        collect(left)
        collect(right)
      case PrefixOp(_, od) =>
        collect(od)
      case Parens(tree) =>
        collect(tree)
      case Tuple(trees) =>
        trees foreach collect
      case Thicket(trees) =>
        trees foreach collect
      case Block(Nil, expr) =>
        collect(expr)
      case Quote(body, _) =>
        new UntypedTreeTraverser {
          def traverse(tree: untpd.Tree)(using Context): Unit = tree match {
            case SplicePattern(body, _, _) => collect(body)
            case _ => traverseChildren(tree)
          }
        }.traverse(body)
      case _ =>
    }
    collect(tree)
    buf.toList
  }
}
