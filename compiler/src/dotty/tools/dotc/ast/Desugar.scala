package dotty.tools
package dotc
package ast

import core._
import util.Spans._, Types._, Contexts._, Constants._, Names._, NameOps._, Flags._
import Symbols._, StdNames._, Trees._
import Decorators._, transform.SymUtils._
import NameKinds.{UniqueName, EvidenceParamName, DefaultGetterName}
import typer.FrontEnd
import util.Property
import collection.mutable.ListBuffer
import reporting.diagnostic.messages._
import reporting.trace
import annotation.transientParam

import scala.annotation.internal.sharable

object desugar {
  import untpd._
  import DesugarEnums._

  /** If a Select node carries this attachment, suppress the check
   *  that its type refers to an acessible symbol.
   */
  val SuppressAccessCheck = new Property.Key[Unit]

  /** Info of a variable in a pattern: The named tree and its type */
  private type VarInfo = (NameTree, Tree)

  /** Is `name` the name of a method that can be invalidated as a compiler-generated
   *  case class method that clashes with a user-defined method?
   */
  def isRetractableCaseClassMethodName(name: Name)(implicit ctx: Context): Boolean = name match {
    case nme.apply | nme.unapply | nme.copy => true
    case DefaultGetterName(nme.copy, _) => true
    case _ => false
  }

  /** Is `name` the name of a method that is added unconditionally to case classes? */
  def isDesugaredCaseClassMethodName(name: Name)(implicit ctx: Context): Boolean =
    isRetractableCaseClassMethodName(name) || name.isSelectorName

// ----- DerivedTypeTrees -----------------------------------

  class SetterParamTree(implicit @transientParam src: SourceInfo) extends DerivedTypeTree {
    def derivedTree(sym: Symbol)(implicit ctx: Context): tpd.TypeTree = tpd.TypeTree(sym.info.resultType)
  }

  class TypeRefTree(implicit @transientParam src: SourceInfo) extends DerivedTypeTree {
    def derivedTree(sym: Symbol)(implicit ctx: Context): tpd.TypeTree = tpd.TypeTree(sym.typeRef)
  }

  class TermRefTree(implicit @transientParam src: SourceInfo) extends DerivedTypeTree {
    def derivedTree(sym: Symbol)(implicit ctx: Context): tpd.Tree = tpd.ref(sym)
  }

  /** A type tree that computes its type from an existing parameter.
   *  @param suffix  String difference between existing parameter (call it `P`) and parameter owning the
   *                 DerivedTypeTree (call it `O`). We have: `O.name == P.name + suffix`.
   */
  class DerivedFromParamTree(suffix: String)(implicit @transientParam src: SourceInfo) extends DerivedTypeTree {

    /** Make sure that for all enclosing module classes their companion classes
     *  are completed. Reason: We need the constructor of such companion classes to
     *  be completed so that OriginalSymbol attachments are pushed to DerivedTypeTrees
     *  in apply/unapply methods.
     */
    override def ensureCompletions(implicit ctx: Context): Unit =
      if (!(ctx.owner is Package))
        if (ctx.owner.isClass) {
          ctx.owner.ensureCompleted()
          if (ctx.owner is ModuleClass)
            ctx.owner.linkedClass.ensureCompleted()
        }
        else ensureCompletions(ctx.outer)

    /** Return info of original symbol, where all references to siblings of the
     *  original symbol (i.e. sibling and original symbol have the same owner)
     *  are rewired to like-named* parameters or accessors in the scope enclosing
     *  the current scope. The current scope is the scope owned by the defined symbol
     *  itself, that's why we have to look one scope further out. If the resulting
     *  type is an alias type, dealias it. This is necessary because the
     *  accessor of a type parameter is a private type alias that cannot be accessed
     *  from subclasses.
     *
     *  (*) like-named means:
     *
     *       parameter name  ==  reference name ++ suffix
     */
    def derivedTree(sym: Symbol)(implicit ctx: Context): tpd.TypeTree = {
      val relocate = new TypeMap {
        val originalOwner = sym.owner
        def apply(tp: Type) = tp match {
          case tp: NamedType if tp.symbol.exists && (tp.symbol.owner eq originalOwner) =>
            val defctx = ctx.outersIterator.dropWhile(_.scope eq ctx.scope).next()
            var local = defctx.denotNamed(tp.name ++ suffix).suchThat(_.isParamOrAccessor).symbol
            if (local.exists) (defctx.owner.thisType select local).dealiasKeepAnnots
            else {
              def msg =
                s"no matching symbol for ${tp.symbol.showLocated} in ${defctx.owner} / ${defctx.effectiveScope.toList}"
              ErrorType(msg).assertingErrorsReported(msg)
            }
          case _ =>
            mapOver(tp)
        }
      }
      tpd.TypeTree(relocate(sym.info))
    }
  }

  /** A type definition copied from `tdef` with a rhs typetree derived from it */
  def derivedTypeParam(tdef: TypeDef, suffix: String = "")(implicit ctx: Context): TypeDef =
    cpy.TypeDef(tdef)(
      name = tdef.name ++ suffix,
      rhs = new DerivedFromParamTree(suffix).withPosOf(tdef.rhs).watching(tdef)
    )

  /** A derived type definition watching `sym` */
  def derivedTypeParam(sym: TypeSymbol)(implicit ctx: Context): TypeDef =
    TypeDef(sym.name, new DerivedFromParamTree("").watching(sym)).withFlags(TypeParam)

  /** A value definition copied from `vdef` with a tpt typetree derived from it */
  def derivedTermParam(vdef: ValDef)(implicit ctx: Context): ValDef =
    cpy.ValDef(vdef)(
      tpt = new DerivedFromParamTree("").withPosOf(vdef.tpt).watching(vdef))

// ----- Desugar methods -------------------------------------------------

  /**   var x: Int = expr
   *  ==>
   *    def x: Int = expr
   *    def x_=($1: <TypeTree()>): Unit = ()
   */
  def valDef(vdef: ValDef)(implicit ctx: Context): Tree = {
    val ValDef(name, tpt, rhs) = vdef
    val mods = vdef.mods
    val setterNeeded =
      (mods is Mutable) && ctx.owner.isClass && (!(mods is PrivateLocal) || (ctx.owner is Trait))
    if (setterNeeded) {
      // TODO: copy of vdef as getter needed?
      // val getter = ValDef(mods, name, tpt, rhs) withPos vdef.pos?
      // right now vdef maps via expandedTree to a thicket which concerns itself.
      // I don't see a problem with that but if there is one we can avoid it by making a copy here.
      val setterParam = makeSyntheticParameter(tpt = (new SetterParamTree).watching(vdef))
      // The rhs gets filled in later, when field is generated and getter has parameters (see Memoize miniphase)
      val setterRhs = if (vdef.rhs.isEmpty) EmptyTree else unitLiteral
      val setter = cpy.DefDef(vdef)(
        name     = name.setterName,
        tparams  = Nil,
        vparamss = (setterParam :: Nil) :: Nil,
        tpt      = TypeTree(defn.UnitType),
        rhs      = setterRhs
      ).withMods((mods | Accessor) &~ CaseAccessor)
      Thicket(vdef, setter)
    }
    else vdef
  }

  def makeImplicitParameters(tpts: List[Tree], forPrimaryConstructor: Boolean = false)(implicit ctx: Context): List[ValDef] =
    for (tpt <- tpts) yield {
       val paramFlags: FlagSet = if (forPrimaryConstructor) PrivateLocalParamAccessor else Param
       val epname = EvidenceParamName.fresh()
       ValDef(epname, tpt, EmptyTree).withFlags(paramFlags | Implicit)
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
   *
   *  3. Convert <: T to : T in specializing inline methods. E.g.
   *
   *      inline def f(x: Boolean) <: Any = if (x) 1 else ""
   *  ==>
   *      inline def f(x: Boolean): Any = if (x) 1 else ""
   *
   *  4. Upcast non-specializing inline methods. E.g.
   *
   *      inline def f(x: Boolean): Any = if (x) 1 else ""
   *  ==>
   *      inline def f(x: Boolean): Any = (if (x) 1 else ""): Any
   */
  private def defDef(meth: DefDef, isPrimaryConstructor: Boolean = false)(implicit ctx: Context): Tree = {
    val DefDef(name, tparams, vparamss, tpt, rhs) = meth
    val mods = meth.mods
    val epbuf = new ListBuffer[ValDef]
    def desugarContextBounds(rhs: Tree): Tree = rhs match {
      case ContextBounds(tbounds, cxbounds) =>
        epbuf ++= makeImplicitParameters(cxbounds, isPrimaryConstructor)
        tbounds
      case LambdaTypeTree(tparams, body) =>
        cpy.LambdaTypeTree(rhs)(tparams, desugarContextBounds(body))
      case _ =>
        rhs
    }
    val tparams1 = tparams mapConserve { tparam =>
      cpy.TypeDef(tparam)(rhs = desugarContextBounds(tparam.rhs))
    }

    var meth1 = addEvidenceParams(cpy.DefDef(meth)(tparams = tparams1), epbuf.toList)

    if (meth1.mods.is(Inline))
      meth1.tpt match {
        case TypeBoundsTree(_, tpt1) =>
          meth1 = cpy.DefDef(meth1)(tpt = tpt1)
        case tpt if !tpt.isEmpty && !meth1.rhs.isEmpty =>
          meth1 = cpy.DefDef(meth1)(rhs = Typed(meth1.rhs, tpt))
        case _ =>
      }

    /** The longest prefix of parameter lists in vparamss whose total length does not exceed `n` */
    def takeUpTo(vparamss: List[List[ValDef]], n: Int): List[List[ValDef]] = vparamss match {
      case vparams :: vparamss1 =>
        val len = vparams.length
        if (n >= len) vparams :: takeUpTo(vparamss1, n - len) else Nil
      case _ =>
        Nil
    }

    def normalizedVparamss = meth1.vparamss map (_ map (vparam =>
      cpy.ValDef(vparam)(rhs = EmptyTree)))

    def dropContextBound(tparam: TypeDef) = tparam.rhs match {
      case ContextBounds(tbounds, _) => cpy.TypeDef(tparam)(rhs = tbounds)
      case _ => tparam
    }

    def defaultGetters(vparamss: List[List[ValDef]], n: Int): List[DefDef] = vparamss match {
      case (vparam :: vparams) :: vparamss1 =>
        def defaultGetter: DefDef =
          DefDef(
            name = DefaultGetterName(meth.name, n),
            tparams = meth.tparams.map(tparam => dropContextBound(toDefParam(tparam))),
            vparamss = takeUpTo(normalizedVparamss.nestedMap(toDefParam), n),
            tpt = TypeTree(),
            rhs = vparam.rhs
          )
          .withMods(Modifiers(mods.flags & (AccessFlags | Synthetic), mods.privateWithin))
        val rest = defaultGetters(vparams :: vparamss1, n + 1)
        if (vparam.rhs.isEmpty) rest else defaultGetter :: rest
      case Nil :: vparamss1 =>
        defaultGetters(vparamss1, n)
      case nil =>
        Nil
    }

    val defGetters = defaultGetters(vparamss, 0)
    if (defGetters.isEmpty) meth1
    else {
      val meth2 = cpy.DefDef(meth1)(vparamss = normalizedVparamss)
        .withMods(meth1.mods | DefaultParameterized)
      Thicket(meth2 :: defGetters)
    }
  }

  // Add all evidence parameters in `params` as implicit parameters to `meth` */
  private def addEvidenceParams(meth: DefDef, params: List[ValDef])(implicit ctx: Context): DefDef =
    params match {
      case Nil =>
        meth
      case evidenceParams =>
        val vparamss1 = meth.vparamss.reverse match {
          case (vparams @ (vparam :: _)) :: rvparamss if vparam.mods is Implicit =>
            ((evidenceParams ++ vparams) :: rvparamss).reverse
          case _ =>
            meth.vparamss :+ evidenceParams
        }
        cpy.DefDef(meth)(vparamss = vparamss1)
    }

  /** The implicit evidence parameters of `meth`, as generated by `desugar.defDef` */
  private def evidenceParams(meth: DefDef)(implicit ctx: Context): List[ValDef] =
    meth.vparamss.reverse match {
      case (vparams @ (vparam :: _)) :: _ if vparam.mods is Implicit =>
        vparams.dropWhile(!_.name.is(EvidenceParamName))
      case _ =>
        Nil
    }

  @sharable private val synthetic = Modifiers(Synthetic)

  private def toDefParam(tparam: TypeDef): TypeDef =
    tparam.withMods(tparam.rawMods & EmptyFlags | Param)
  private def toDefParam(vparam: ValDef): ValDef =
    vparam.withMods(vparam.rawMods & (Implicit | Erased) | Param)

  /** The expansion of a class definition. See inline comments for what is involved */
  def classDef(cdef: TypeDef)(implicit ctx: Context): Tree = {
    val className = checkNotReservedName(cdef).asTypeName
    val impl @ Template(constr0, parents, self, _) = cdef.rhs
    val mods = cdef.mods
    val companionMods = mods
        .withFlags((mods.flags & (AccessFlags | Final)).toCommonFlags)
        .withMods(Nil)

    var defaultGetters: List[Tree] = Nil

    def decompose(ddef: Tree): DefDef = ddef match {
      case meth: DefDef => meth
      case Thicket((meth: DefDef) :: defaults) =>
        defaultGetters = defaults
        meth
    }

    val constr1 = decompose(defDef(constr0, isPrimaryConstructor = true))

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
      case Ident(nme.scala_) => true
      case Select(Ident(nme.ROOTPKG), nme.scala_) => true
      case _ => false
    }

    val isCaseClass  = mods.is(Case) && !mods.is(Module)
    val isCaseObject = mods.is(Case) && mods.is(Module)
    val isImplicit = mods.is(Implicit)
    val isEnum = mods.isEnumClass && !mods.is(Module)
    def isEnumCase = mods.isEnumCase
    val isValueClass = parents.nonEmpty && isAnyVal(parents.head)
      // This is not watertight, but `extends AnyVal` will be replaced by `inline` later.

    val originalTparams = constr1.tparams
    val originalVparamss = constr1.vparamss
    lazy val derivedEnumParams = enumClass.typeParams.map(derivedTypeParam)
    val impliedTparams =
      if (isEnumCase && originalTparams.isEmpty)
        derivedEnumParams.map(tdef => tdef.withFlags(tdef.mods.flags | PrivateLocal))
      else
        originalTparams
    val constrTparams = impliedTparams.map(toDefParam)
    val constrVparamss =
      if (originalVparamss.isEmpty) { // ensure parameter list is non-empty
        if (isCaseClass && originalTparams.isEmpty)
          ctx.error(CaseClassMissingParamList(cdef), cdef.sourcePos.withSpan(cdef.namePos))
        ListOfNil
      } else if (isCaseClass && originalVparamss.head.exists(_.mods.is(Implicit))) {
          ctx.error("Case classes should have a non-implicit parameter list", cdef.sourcePos.withSpan(cdef.namePos))
        ListOfNil
      }
      else originalVparamss.nestedMap(toDefParam)
    val constr = cpy.DefDef(constr1)(tparams = constrTparams, vparamss = constrVparamss)

    val (normalizedBody, enumCases, enumCompanionRef) = {
      // Add constructor type parameters and evidence implicit parameters
      // to auxiliary constructors; set defaultGetters as a side effect.
      def expandConstructor(tree: Tree) = tree match {
        case ddef: DefDef if ddef.name.isConstructorName =>
          decompose(
            defDef(
              addEvidenceParams(
                cpy.DefDef(ddef)(tparams = constrTparams),
                evidenceParams(constr1).map(toDefParam))))
        case stat =>
          stat
      }
      // The Identifiers defined by a case
      def caseIds(tree: Tree) = tree match {
        case tree: MemberDef => Ident(tree.name.toTermName) :: Nil
        case PatDef(_, ids, _, _) => ids
      }
      val stats = impl.body.map(expandConstructor)
      if (isEnum) {
        val (enumCases, enumStats) = stats.partition(DesugarEnums.isEnumCase)
        val enumCompanionRef = new TermRefTree()
        val enumImport = Import(enumCompanionRef, enumCases.flatMap(caseIds))
        (enumImport :: enumStats, enumCases, enumCompanionRef)
      }
      else (stats, Nil, EmptyTree)
    }

    def anyRef = ref(defn.AnyRefAlias.typeRef)

    val derivedTparams = constrTparams.map(derivedTypeParam(_))
    val derivedVparamss = constrVparamss.nestedMap(derivedTermParam(_))
    val arity = constrVparamss.head.length

    val classTycon: Tree = new TypeRefTree // watching is set at end of method

    def appliedTypeTree(tycon: Tree, args: List[Tree]) =
      (if (args.isEmpty) tycon else AppliedTypeTree(tycon, args))
        .withSpan(cdef.pos.startPos)

    def isHK(tparam: Tree): Boolean = tparam match {
      case TypeDef(_, LambdaTypeTree(tparams, body)) => true
      case TypeDef(_, rhs: DerivedTypeTree) => isHK(rhs.watched)
      case _ => false
    }

    def appliedRef(tycon: Tree, tparams: List[TypeDef] = constrTparams, widenHK: Boolean = false) = {
      val targs = for (tparam <- tparams) yield {
        val targ = refOfDef(tparam)
        def fullyApplied(tparam: Tree): Tree = tparam match {
          case TypeDef(_, LambdaTypeTree(tparams, body)) =>
            AppliedTypeTree(targ, tparams.map(_ => TypeBoundsTree(EmptyTree, EmptyTree)))
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
        ctx.error(i"explicit extends clause needed because both enum case and enum class have type parameters"
            , cdef.sourcePos.startPos)
        appliedTypeTree(enumClassRef, constrTparams map (_ => anyRef))
      }

    // new C[Ts](paramss)
    lazy val creatorExpr = New(classTypeRef, constrVparamss nestedMap refOfDef)

    val copiedAccessFlags = if (ctx.scala2Setting) EmptyFlags else AccessFlags

    // Methods to add to a case class C[..](p1: T1, ..., pN: Tn)(moreParams)
    //     def _1: T1 = this.p1
    //     ...
    //     def _N: TN = this.pN
    //     def copy(p1: T1 = p1: @uncheckedVariance, ...,
    //              pN: TN = pN: @uncheckedVariance)(moreParams) =
    //       new C[...](p1, ..., pN)(moreParams)
    //
    // Note: copy default parameters need @uncheckedVariance; see
    // neg/t1843-variances.scala for a test case. The test would give
    // two errors without @uncheckedVariance, one of them spurious.
    val caseClassMeths = {
      def syntheticProperty(name: TermName, tpt: Tree, rhs: Tree) =
        DefDef(name, Nil, Nil, tpt, rhs).withMods(synthetic)
      def productElemMeths = {
        val caseParams = derivedVparamss.head.toArray
        for (i <- List.range(0, arity) if nme.selectorName(i) `ne` caseParams(i).name)
        yield syntheticProperty(nme.selectorName(i), caseParams(i).tpt,
          Select(This(EmptyTypeIdent), caseParams(i).name))
      }
      def enumTagMeths = if (isEnumCase) enumTagMeth(CaseKind.Class)._1 :: Nil else Nil
      def copyMeths = {
        def isRepeated(tree: Tree): Boolean = tree match {
          case PostfixOp(_, Ident(tpnme.raw.STAR)) => true
          case ByNameTypeTree(tree1) => isRepeated(tree1)
          case _ => false
        }
        val hasRepeatedParam = constrVparamss.exists(_.exists {
          case ValDef(_, tpt, _) => isRepeated(tpt)
        })
        if (mods.is(Abstract) || hasRepeatedParam) Nil  // cannot have default arguments for repeated parameters, hence copy method is not issued
        else {
          def copyDefault(vparam: ValDef) =
            makeAnnotated("scala.annotation.unchecked.uncheckedVariance", refOfDef(vparam))
          val copyFirstParams = derivedVparamss.head.map(vparam =>
            cpy.ValDef(vparam)(rhs = copyDefault(vparam)))
          val copyRestParamss = derivedVparamss.tail.nestedMap(vparam =>
            cpy.ValDef(vparam)(rhs = EmptyTree))
          DefDef(nme.copy, derivedTparams, copyFirstParams :: copyRestParamss, TypeTree(), creatorExpr)
            .withFlags(Synthetic | constr1.mods.flags & copiedAccessFlags) :: Nil
        }
      }

      // TODO When the Scala library is updated to 2.13.x add the override keyword to this generated method.
      // (because Product.scala was updated)
      def productElemNameMeth = {
        val methodParam = makeSyntheticParameter(tpt = scalaDot(tpnme.Int))
        val paramRef = Ident(methodParam.name)

        val indexAsString = Apply(Select(javaDotLangDot(nme.String), nme.valueOf), paramRef)
        val throwOutOfBound = Throw(New(javaDotLangDot(tpnme.IOOBException), List(List(indexAsString))))
        val defaultCase = CaseDef(Ident(nme.WILDCARD), EmptyTree, throwOutOfBound)

        val patternMatchCases = derivedVparamss.head.zipWithIndex.map { case (param, idx) =>
            CaseDef(Literal(Constant(idx)), EmptyTree, Literal(Constant(param.name.decode.toString)))
        } :+ defaultCase
        val body = Match(paramRef, patternMatchCases)
        DefDef(nme.productElementName, Nil, List(List(methodParam)), javaDotLangDot(tpnme.String), body)
          .withFlags(if (defn.isNewCollections) Override | Synthetic else Synthetic)
      }

      if (isCaseClass)
        productElemNameMeth :: copyMeths ::: enumTagMeths ::: productElemMeths
      else Nil
    }

    // Case classes and case objects get Product parents
    // Enum cases get an inferred parent if no parents are given
    var parents1 = parents
    if (isEnumCase && parents.isEmpty)
      parents1 = enumClassTypeRef :: Nil
    if (isCaseClass | isCaseObject)
      parents1 = parents1 :+ scalaDot(str.Product.toTypeName)
    if (isEnum)
      parents1 = parents1 :+ ref(defn.EnumType)

    // The Eq instance for an Enum class. For an enum class
    //
    //    enum class C[T1, ..., Tn]
    //
    // we generate:
    //
    //    implicit def eqInstance[T1$1, ..., Tn$1, T1$2, ..., Tn$2](implicit
    //      ev1: Eq[T1$1, T1$2], ..., evn: Eq[Tn$1, Tn$2]])
    //      : Eq[C[T1$, ..., Tn$1], C[T1$2, ..., Tn$2]] = Eq
    //
    // Higher-kinded type arguments `Ti` are omitted as evidence parameters.
    //
    // FIXME: This is too simplistic. Instead of just generating evidence arguments
    // for every first-kinded type parameter, we should look instead at the
    // actual types occurring in cases and derive parameters from these. E.g. in
    //
    //    enum HK[F[_]] {
    //      case C1(x: F[Int]) extends HK[F[Int]]
    //      case C2(y: F[String]) extends HL[F[Int]]
    //
    // we would need evidence parameters for `F[Int]` and `F[String]`
    // We should generate Eq instances with the techniques
    // of typeclass derivation once that is available.
    def eqInstance = {
      val leftParams = constrTparams.map(derivedTypeParam(_, "$1"))
      val rightParams = constrTparams.map(derivedTypeParam(_, "$2"))
      val subInstances =
        for ((param1, param2) <- leftParams `zip` rightParams if !isHK(param1))
        yield appliedRef(ref(defn.EqType), List(param1, param2), widenHK = true)
      DefDef(
          name = nme.eqInstance,
          tparams = leftParams ++ rightParams,
          vparamss = if (subInstances.isEmpty) Nil else List(makeImplicitParameters(subInstances)),
          tpt = appliedTypeTree(ref(defn.EqType),
              appliedRef(classTycon, leftParams) :: appliedRef(classTycon, rightParams) :: Nil),
          rhs = ref(defn.EqModule.termRef)).withFlags(Synthetic | Implicit)
    }
    def eqInstances = if (isEnum) eqInstance :: Nil else Nil

    // The thicket which is the desugared version of the companion object
    //     synthetic object C extends parentTpt { defs }
    def companionDefs(parentTpt: Tree, defs: List[Tree]) =
      moduleDef(
        ModuleDef(
          className.toTermName, Template(emptyConstructor, parentTpt :: Nil, EmptyValDef, defs))
            .withMods(companionMods | Synthetic))
      .withPosOf(cdef).toList

    val companionMembers = defaultGetters ::: eqInstances ::: enumCases

    // The companion object definitions, if a companion is needed, Nil otherwise.
    // companion definitions include:
    // 1. If class is a case class case class C[Ts](p1: T1, ..., pN: TN)(moreParams):
    //     def apply[Ts](p1: T1, ..., pN: TN)(moreParams) = new C[Ts](p1, ..., pN)(moreParams)  (unless C is abstract)
    //     def unapply[Ts]($1: C[Ts]) = $1
    // 2. The default getters of the constructor
    // The parent of the companion object of a non-parameterized case class
    //     (T11, ..., T1N) => ... => (TM1, ..., TMN) => C
    // For all other classes, the parent is AnyRef.
    val companions =
      if (isCaseClass) {
        // The return type of the `apply` method, and an (empty or singleton) list
        // of widening coercions
        val (applyResultTpt, widenDefs) =
          if (!isEnumCase)
            (TypeTree(), Nil)
          else if (parents.isEmpty || enumClass.typeParams.isEmpty)
            (enumClassTypeRef, Nil)
          else
            enumApplyResult(cdef, parents, derivedEnumParams, appliedRef(enumClassRef, derivedEnumParams))

        val companionParent =
          if (constrTparams.nonEmpty ||
              constrVparamss.length > 1 ||
              mods.is(Abstract) ||
              constr.mods.is(Private)) anyRef
          else
            // todo: also use anyRef if constructor has a dependent method type (or rule that out)!
            (constrVparamss :\ (if (isEnumCase) applyResultTpt else classTypeRef)) (
              (vparams, restpe) => Function(vparams map (_.tpt), restpe))
        def widenedCreatorExpr =
          (creatorExpr /: widenDefs)((rhs, meth) => Apply(Ident(meth.name), rhs :: Nil))
        val applyMeths =
          if (mods is Abstract) Nil
          else
            DefDef(nme.apply, derivedTparams, derivedVparamss, applyResultTpt, widenedCreatorExpr)
              .withFlags(Synthetic | constr1.mods.flags & (DefaultParameterized | copiedAccessFlags)) :: widenDefs
        val unapplyMeth = {
          val unapplyParam = makeSyntheticParameter(tpt = classTypeRef)
          val unapplyRHS = if (arity == 0) Literal(Constant(true)) else Ident(unapplyParam.name)
          DefDef(nme.unapply, derivedTparams, (unapplyParam :: Nil) :: Nil, TypeTree(), unapplyRHS)
            .withMods(synthetic)
        }
        companionDefs(companionParent, applyMeths ::: unapplyMeth :: companionMembers)
      }
      else if (companionMembers.nonEmpty)
        companionDefs(anyRef, companionMembers)
      else if (isValueClass) {
        constr0.vparamss match {
          case (_ :: Nil) :: _ => companionDefs(anyRef, Nil)
          case _ => Nil // error will be emitted in typer
        }
      }
      else Nil

    enumCompanionRef match {
      case ref: TermRefTree => // have the enum import watch the companion object
        val (modVal: ValDef) :: _ = companions
        ref.watching(modVal)
      case _ =>
    }

    // For an implicit class C[Ts](p11: T11, ..., p1N: T1N) ... (pM1: TM1, .., pMN: TMN), the method
    //     synthetic implicit C[Ts](p11: T11, ..., p1N: T1N) ... (pM1: TM1, ..., pMN: TMN): C[Ts] =
    //       new C[Ts](p11, ..., p1N) ... (pM1, ..., pMN) =
    val implicitWrappers =
      if (!isImplicit)
        Nil
      else if (ctx.owner is Package) {
        ctx.error(TopLevelImplicitClass(cdef), cdef.sourcePos)
        Nil
      }
      else if (isCaseClass) {
        ctx.error(ImplicitCaseClass(cdef), cdef.sourcePos)
        Nil
      }
      else if (arity != 1) {
        ctx.error(ImplicitClassPrimaryConstructorArity(), cdef.sourcePos)
        Nil
      }
      else
        // implicit wrapper is typechecked in same scope as constructor, so
        // we can reuse the constructor parameters; no derived params are needed.
        DefDef(className.toTermName, constrTparams, constrVparamss, classTypeRef, creatorExpr)
          .withMods(companionMods | Synthetic | Implicit)
          .withPosOf(cdef) :: Nil

    val self1 = {
      val selfType = if (self.tpt.isEmpty) classTypeRef else self.tpt
      if (self.isEmpty) self
      else cpy.ValDef(self)(tpt = selfType).withMods(self.mods | SelfName)
    }

    val cdef1 = addEnumFlags {
      val originalTparamsIt = impliedTparams.toIterator
      val originalVparamsIt = originalVparamss.toIterator.flatten
      val tparamAccessors = derivedTparams.map(_.withMods(originalTparamsIt.next().mods))
      val caseAccessor = if (isCaseClass) CaseAccessor else EmptyFlags
      val vparamAccessors = derivedVparamss match {
        case first :: rest =>
          first.map(_.withMods(originalVparamsIt.next().mods | caseAccessor)) ++
          rest.flatten.map(_.withMods(originalVparamsIt.next().mods))
        case _ =>
          Nil
      }
      cpy.TypeDef(cdef: TypeDef)(
        name = className,
        rhs = cpy.Template(impl)(constr, parents1, self1,
          tparamAccessors ::: vparamAccessors ::: normalizedBody ::: caseClassMeths)): TypeDef
    }

    // install the watch on classTycon
    classTycon match {
      case tycon: DerivedTypeTree => tycon.watching(cdef1)
      case _ =>
    }

    flatTree(cdef1 :: companions ::: implicitWrappers)
  }

  /** Expand
   *
   *    object name extends parents { self => body }
   *
   *  to:
   *    <module> val name: name$ = New(name$)
   *    <module> final class name$ extends parents { self: name.type => body }
   */
  def moduleDef(mdef: ModuleDef)(implicit ctx: Context): Tree = {
    val moduleName = checkNotReservedName(mdef).asTermName
    val impl = mdef.impl
    val mods = mdef.mods
    def isEnumCase = mods.isEnumCase
    if (mods is Package)
      PackageDef(Ident(moduleName), cpy.ModuleDef(mdef)(nme.PACKAGE, impl).withMods(mods &~ Package) :: Nil)
    else if (isEnumCase)
      expandEnumModule(moduleName, impl, mods, mdef.pos)
    else {
      val clsName = moduleName.moduleClassName
      val clsRef = Ident(clsName)
      val modul = ValDef(moduleName, clsRef, New(clsRef, Nil))
        .withMods(mods.toTermFlags & RetainedModuleValFlags | ModuleValCreationFlags)
        .withSpan(mdef.pos.startPos)
      val ValDef(selfName, selfTpt, _) = impl.self
      val selfMods = impl.self.mods
      if (!selfTpt.isEmpty) ctx.error(ObjectMayNotHaveSelfType(mdef), impl.self.sourcePos)
      val clsSelf = ValDef(selfName, SingletonTypeTree(Ident(moduleName)), impl.self.rhs)
        .withMods(selfMods)
        .withSpan(impl.self.pos.orElse(impl.pos.startPos))
      val clsTmpl = cpy.Template(impl)(self = clsSelf, body = impl.body)
      val cls = TypeDef(clsName, clsTmpl)
        .withMods(mods.toTypeFlags & RetainedModuleClassFlags | ModuleClassCreationFlags)
      Thicket(modul, classDef(cls).withPosOf(mdef))
    }
  }

  /** Expand
   *
   *    <mods> opaque type T = [Xs] => R
   *
   *  to
   *
   *    <mods> opaque type T = T.T
   *    synthetic object T {
   *      synthetic opaque type T >: [Xs] => R
   *    }
   *
   *  The generated companion object will later (in Namer) be merged with the user-defined
   *  companion object, and the synthetic opaque type member will go into the self type.
   */
  def opaqueAlias(tdef: TypeDef)(implicit ctx: Context): Tree =
    if (tdef.rhs.isInstanceOf[TypeBoundsTree]) {
      ctx.error(em"opaque type ${tdef.name} must be an alias type", tdef.sourcePos)
      tdef.withFlags(tdef.mods.flags &~ Opaque)
    }
    else {
      def completeForwarder(fwd: Tree) = tdef.rhs match {
        case LambdaTypeTree(tparams, tpt) =>
          val tparams1 =
            for (tparam <- tparams)
            yield tparam.withMods(tparam.mods | Synthetic)
          lambdaAbstract(tparams1,
            AppliedTypeTree(fwd, tparams.map(tparam => Ident(tparam.name))))
        case _ =>
          fwd
      }
      val moduleName = tdef.name.toTermName
      val localRef = Select(Ident(moduleName), tdef.name)
      localRef.pushAttachment(SuppressAccessCheck, ())
      val aliasType = cpy.TypeDef(tdef)(rhs = completeForwarder(localRef))
      val localType = tdef.withMods(Modifiers(Synthetic | Opaque).withPrivateWithin(tdef.name))

      val companions = moduleDef(ModuleDef(
        moduleName, Template(emptyConstructor, Nil, EmptyValDef, localType :: Nil))
          .withFlags(Synthetic | Opaque))
      Thicket(aliasType :: companions.toList)
    }

  /** The name of `mdef`, after checking that it does not redefine a Scala core class.
   *  If it does redefine, issue an error and return a mangled name instead of the original one.
   */
  def checkNotReservedName(mdef: MemberDef)(implicit ctx: Context): Name = {
    val name = mdef.name
    if (ctx.owner == defn.ScalaPackageClass && defn.reservedScalaClassNames.contains(name.toTypeName)) {
      def kind = if (name.isTypeName) "class" else "object"
      ctx.error(em"illegal redefinition of standard $kind $name", mdef.sourcePos)
      name.errorName
    }
    else name
  }

  /**     val p1, ..., pN: T = E
   *  ==>
   *      makePatDef[[val p1: T1 = E]]; ...; makePatDef[[val pN: TN = E]]
   *
   *      case e1, ..., eN
   *  ==>
   *      expandSimpleEnumCase([case e1]); ...; expandSimpleEnumCase([case eN])
   */
  def patDef(pdef: PatDef)(implicit ctx: Context): Tree = flatTree {
    val PatDef(mods, pats, tpt, rhs) = pdef
    if (mods.isEnumCase)
      pats map {
        case id: Ident =>
          expandSimpleEnumCase(id.name.asTermName, mods,
            Span(pdef.pos.start, id.pos.end, id.pos.start))
    }
    else {
      val pats1 = if (tpt.isEmpty) pats else pats map (Typed(_, tpt))
      pats1 map (makePatDef(pdef, mods, _, rhs))
    }
  }

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
  def makePatDef(original: Tree, mods: Modifiers, pat: Tree, rhs: Tree)(implicit ctx: Context): Tree = pat match {
    case IdPattern(named, tpt) =>
      derivedValDef(original, named, tpt, rhs, mods)
    case _ =>
      val rhsUnchecked = makeAnnotated("scala.unchecked", rhs)
      val vars = getVariables(pat)
      val isMatchingTuple: Tree => Boolean = {
        case Tuple(es) => es.length == vars.length
        case _ => false
      }
      val ids = for ((named, _) <- vars) yield Ident(named.name)
      val caseDef = CaseDef(pat, EmptyTree, makeTuple(ids))
      val matchExpr =
        if (forallResults(rhs, isMatchingTuple)) rhs
        else Match(rhsUnchecked, caseDef :: Nil)
      vars match {
        case Nil =>
          matchExpr
        case (named, tpt) :: Nil =>
          derivedValDef(original, named, tpt, matchExpr, mods)
        case _ =>
          val tmpName = UniqueName.fresh()
          val patMods =
            mods & Lazy | Synthetic | (if (ctx.owner.isClass) PrivateLocal else EmptyFlags)
          val firstDef =
            ValDef(tmpName, TypeTree(), matchExpr)
              .withSpan(pat.pos.union(rhs.pos)).withMods(patMods)
          def selector(n: Int) = Select(Ident(tmpName), nme.selectorName(n))
          val restDefs =
            for (((named, tpt), n) <- vars.zipWithIndex)
            yield
              if (mods is Lazy) derivedDefDef(original, named, tpt, selector(n), mods &~ Lazy)
              else derivedValDef(original, named, tpt, selector(n), mods)
          flatTree(firstDef :: restDefs)
      }
  }

  /** Expand variable identifier x to x @ _ */
  def patternVar(tree: Tree)(implicit ctx: Context): Bind = {
    val Ident(name) = tree
    Bind(name, Ident(nme.WILDCARD)).withPosOf(tree)
  }

  def defTree(tree: Tree)(implicit ctx: Context): Tree = tree match {
    case tree: ValDef => valDef(tree)
    case tree: TypeDef =>
      if (tree.isClassDef) classDef(tree)
      else if (tree.mods.is(Opaque, butNot = Synthetic)) opaqueAlias(tree)
      else tree
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
  def block(tree: Block)(implicit ctx: Context): Block = tree.expr match {
    case EmptyTree =>
      cpy.Block(tree)(tree.stats,
        unitLiteral.withSpan(if (tree.stats.isEmpty) tree.pos else tree.pos.endPos))
    case _ =>
      tree
  }

  /** Translate infix operation expression
    *
    *     l op r     ==>    l.op(r)  if op is left-associative
    *                ==>    r.op(l)  if op is right-associative
    */
  def binop(left: Tree, op: Ident, right: Tree)(implicit ctx: Context): Apply = {
    def assignToNamedArg(arg: Tree) = arg match {
      case Assign(Ident(name), rhs) => cpy.NamedArg(arg)(name, rhs)
      case _ => arg
    }
    def makeOp(fn: Tree, arg: Tree, selectPos: Span) = {
      val args: List[Tree] = arg match {
        case Parens(arg) => assignToNamedArg(arg) :: Nil
        case Tuple(args) => args.mapConserve(assignToNamedArg)
        case _ => arg :: Nil
      }
      Apply(Select(fn, op.name).withSpan(selectPos), args)
    }

    if (isLeftAssoc(op.name))
      makeOp(left, right, Span(left.pos.start, op.pos.end, op.pos.start))
    else
      makeOp(right, left, Span(op.pos.start, right.pos.end))
  }

  /** Translate tuple expressions of arity <= 22
   *
   *     ()             ==>   ()
   *     (t)            ==>   t
   *     (t1, ..., tN)  ==>   TupleN(t1, ..., tN)
   */
  def smallTuple(tree: Tuple)(implicit ctx: Context): Tree = {
    val ts = tree.trees
    val arity = ts.length
    assert(arity <= Definitions.MaxTupleArity)
    def tupleTypeRef = defn.TupleType(arity)
    if (arity == 1) ts.head
    else if (arity == 0)
      if (ctx.mode is Mode.Type) TypeTree(defn.UnitType) else unitLiteral
    else if (ctx.mode is Mode.Type) AppliedTypeTree(ref(tupleTypeRef), ts)
    else Apply(ref(tupleTypeRef.classSymbol.companionModule.termRef), ts)
  }

  /** Make closure corresponding to function.
   *      params => body
   *  ==>
   *      def $anonfun(params) = body
   *      Closure($anonfun)
   */
  def makeClosure(params: List[ValDef], body: Tree, tpt: Tree = null, isImplicit: Boolean)(implicit ctx: Context): Block =
    Block(
      DefDef(nme.ANON_FUN, Nil, params :: Nil, if (tpt == null) TypeTree() else tpt, body)
        .withMods(synthetic | Artifact),
      Closure(Nil, Ident(nme.ANON_FUN), if (isImplicit) ImplicitEmptyTree else EmptyTree))

  /** If `nparams` == 1, expand partial function
   *
   *       { cases }
   *  ==>
   *       x$1 => (x$1 @unchecked) match { cases }
   *
   *  If `nparams` != 1, expand instead to
   *
   *       (x$1, ..., x$n) => (x$0, ..., x${n-1} @unchecked) match { cases }
   */
  def makeCaseLambda(cases: List[CaseDef], nparams: Int = 1, unchecked: Boolean = true)(implicit ctx: Context): Function = {
    val params = (1 to nparams).toList.map(makeSyntheticParameter(_))
    val selector = makeTuple(params.map(p => Ident(p.name)))

    if (unchecked)
      Function(params, Match(Annotated(selector, New(ref(defn.UncheckedAnnotType))), cases))
    else
      Function(params, Match(selector, cases))
  }

  /** Map n-ary function `(p1, ..., pn) => body` where n != 1 to unary function as follows:
   *
   *    x$1 => {
   *      def p1 = x$1._1
   *      ...
   *      def pn = x$1._n
   *      body
   *    }
   *
   *  or if `isGenericTuple`
   *
   *    x$1 => {
   *      def p1 = x$1.apply(0)
   *      ...
   *      def pn = x$1.apply(n-1)
   *      body
   *    }
   */
  def makeTupledFunction(params: List[ValDef], body: Tree, isGenericTuple: Boolean)(implicit ctx: Context): Tree = {
    val param = makeSyntheticParameter()
    def selector(n: Int) =
      if (isGenericTuple) Apply(Select(refOfDef(param), nme.apply), Literal(Constant(n)))
      else Select(refOfDef(param), nme.selectorName(n))
    val vdefs =
      params.zipWithIndex.map{
        case (param, idx) =>
          DefDef(param.name, Nil, Nil, TypeTree(), selector(idx)).withPosOf(param)
      }
    Function(param :: Nil, Block(vdefs, body))
  }

  def makeImplicitFunction(formals: List[Type], body: Tree)(implicit ctx: Context): Tree = {
    val params = makeImplicitParameters(formals.map(TypeTree))
    new FunctionWithMods(params, body, Modifiers(Implicit))
  }

  /** Add annotation to tree:
   *      tree @fullName
   *
   *  The annotation is usually represented as a TypeTree referring to the class
   *  with the given name `fullName`. However, if the annotation matches a file name
   *  that is still to be entered, the annotation is represented as a cascade of `Selects`
   *  following `fullName`. This is necessary so that we avoid reading an annotation from
   *  the classpath that is also compiled from source.
   */
  def makeAnnotated(fullName: String, tree: Tree)(implicit ctx: Context): Annotated = {
    val parts = fullName.split('.')
    val ttree = ctx.typerPhase match {
      case phase: FrontEnd if phase.stillToBeEntered(parts.last) =>
        val prefix =
          ((Ident(nme.ROOTPKG): Tree) /: parts.init)((qual, name) =>
            Select(qual, name.toTermName))
        Select(prefix, parts.last.toTypeName)
      case _ =>
        TypeTree(ctx.requiredClass(fullName).typeRef)
    }
    Annotated(tree, New(ttree, Nil))
  }

  private def derivedValDef(original: Tree, named: NameTree, tpt: Tree, rhs: Tree, mods: Modifiers)(implicit ctx: Context) = {
    val vdef = ValDef(named.name.asTermName, tpt, rhs)
      .withMods(mods)
      .withSpan(original.pos.withPoint(named.pos.start))
    val mayNeedSetter = valDef(vdef)
    mayNeedSetter
   }

  private def derivedDefDef(original: Tree, named: NameTree, tpt: Tree, rhs: Tree, mods: Modifiers)(implicit src: SourceInfo) =
    DefDef(named.name.asTermName, Nil, Nil, tpt, rhs)
      .withMods(mods)
      .withSpan(original.pos.withPoint(named.pos.start))

  /** Main desugaring method */
  def apply(tree: Tree)(implicit ctx: Context): Tree = {

    /** Create tree for for-comprehension `<for (enums) do body>` or
     *   `<for (enums) yield body>` where mapName and flatMapName are chosen
     *  corresponding to whether this is a for-do or a for-yield.
     *  The creation performs the following rewrite rules:
     *
     *  1.
     *
     *    for (P <- G) E   ==>   G.foreach (P => E)
     *
     *     Here and in the following (P => E) is interpreted as the function (P => E)
     *     if P is a variable pattern and as the partial function { case P => E } otherwise.
     *
     *  2.
     *
     *    for (P <- G) yield E  ==>  G.map (P => E)
     *
     *  3.
     *
     *    for (P_1 <- G_1; P_2 <- G_2; ...) ...
     *      ==>
     *    G_1.flatMap (P_1 => for (P_2 <- G_2; ...) ...)
     *
     *  4.
     *
     *    for (P <- G; E; ...) ...
     *      =>
     *    for (P <- G.filter (P => E); ...) ...
     *
     *  5. For any N:
     *
     *    for (P_1 <- G; P_2 = E_2; val P_N = E_N; ...)
     *      ==>
     *    for (TupleN(P_1, P_2, ... P_N) <-
     *      for (x_1 @ P_1 <- G) yield {
     *        val x_2 @ P_2 = E_2
     *        ...
     *        val x_N & P_N = E_N
     *        TupleN(x_1, ..., x_N)
     *      } ...)
     *
     *    If any of the P_i are variable patterns, the corresponding `x_i @ P_i` is not generated
     *    and the variable constituting P_i is used instead of x_i
     *
     *  @param mapName      The name to be used for maps (either map or foreach)
     *  @param flatMapName  The name to be used for flatMaps (either flatMap or foreach)
     *  @param enums        The enumerators in the for expression
     *  @param body         The body of the for expression
     */
    def makeFor(mapName: TermName, flatMapName: TermName, enums: List[Tree], body: Tree): Tree = trace(i"make for ${ForYield(enums, body)}", show = true) {

      /** Make a function value pat => body.
       *  If pat is a var pattern id: T then this gives (id: T) => body
       *  Otherwise this gives { case pat => body }
       */
      def makeLambda(pat: Tree, body: Tree): Tree = pat match {
        case IdPattern(named, tpt) =>
          Function(derivedValDef(pat, named, tpt, EmptyTree, Modifiers(Param)) :: Nil, body)
        case _ =>
          makeCaseLambda(CaseDef(pat, EmptyTree, body) :: Nil)
      }

      /** If `pat` is not an Identifier, a Typed(Ident, _), or a Bind, wrap
       *  it in a Bind with a fresh name. Return the transformed pattern, and the identifier
       *  that refers to the bound variable for the pattern.
       */
      def makeIdPat(pat: Tree): (Tree, Ident) = pat match {
        case Bind(name, _) => (pat, Ident(name))
        case id: Ident if isVarPattern(id) && id.name != nme.WILDCARD => (id, id)
        case Typed(id: Ident, _) if isVarPattern(id) && id.name != nme.WILDCARD => (pat, id)
        case _ =>
          val name = UniqueName.fresh()
          (Bind(name, pat), Ident(name))
      }

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
        Apply(Select(rhs, nme.withFilter), makeCaseLambda(cases))
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

      def isIrrefutableGenFrom(gen: GenFrom): Boolean =
        gen.isInstanceOf[IrrefutableGenFrom] ||
        IdPattern.unapply(gen.pat).isDefined ||
        isIrrefutable(gen.pat, gen.expr)

      /** rhs.name with a pattern filter on rhs unless `pat` is irrefutable when
       *  matched against `rhs`.
       */
      def rhsSelect(gen: GenFrom, name: TermName) = {
        val rhs = if (isIrrefutableGenFrom(gen)) gen.expr else makePatFilter(gen.expr, gen.pat)
        Select(rhs, name)
      }

      enums match {
        case (gen: GenFrom) :: Nil =>
          Apply(rhsSelect(gen, mapName), makeLambda(gen.pat, body))
        case (gen: GenFrom) :: (rest @ (GenFrom(_, _) :: _)) =>
          val cont = makeFor(mapName, flatMapName, rest, body)
          Apply(rhsSelect(gen, flatMapName), makeLambda(gen.pat, cont))
        case (GenFrom(pat, rhs)) :: (rest @ GenAlias(_, _) :: _) =>
          val (valeqs, rest1) = rest.span(_.isInstanceOf[GenAlias])
          val pats = valeqs map { case GenAlias(pat, _) => pat }
          val rhss = valeqs map { case GenAlias(_, rhs) => rhs }
          val (defpat0, id0) = makeIdPat(pat)
          val (defpats, ids) = (pats map makeIdPat).unzip
          val pdefs = (valeqs, defpats, rhss).zipped.map(makePatDef(_, Modifiers(), _, _))
          val rhs1 = makeFor(nme.map, nme.flatMap, GenFrom(defpat0, rhs) :: Nil, Block(pdefs, makeTuple(id0 :: ids)))
          val allpats = pat :: pats
          val vfrom1 = new IrrefutableGenFrom(makeTuple(allpats), rhs1)
          makeFor(mapName, flatMapName, vfrom1 :: rest1, body)
        case (gen: GenFrom) :: test :: rest =>
          val filtered = Apply(rhsSelect(gen, nme.withFilter), makeLambda(gen.pat, test))
          val genFrom =
            if (isIrrefutableGenFrom(gen)) new IrrefutableGenFrom(gen.pat, filtered)
            else GenFrom(gen.pat, filtered)
          makeFor(mapName, flatMapName, genFrom :: rest, body)
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
        Literal(Constant(scala.Symbol(str)))
      case Quote(expr) =>
        if (expr.isType)
          TypeApply(ref(defn.QuotedType_applyR), List(expr))
        else
          Apply(ref(defn.QuotedExpr_applyR), expr)
      case InterpolatedString(id, segments) =>
        val strs = segments map {
          case ts: Thicket => ts.trees.head
          case t => t
        }
        val elems = segments flatMap {
          case ts: Thicket => ts.trees.tail
          case t => Nil
        } map {
          case Block(Nil, expr) => expr // important for interpolated string as patterns, see i1773.scala
          case t => t
        }
        // This is a deliberate departure from scalac, where StringContext is not rooted (See #4732)
        Apply(Select(Apply(scalaDot(nme.StringContext), strs), id), elems)
      case InfixOp(l, op, r) =>
        if (ctx.mode is Mode.Type)
          AppliedTypeTree(op, l :: r :: Nil) // op[l, r]
        else {
          assert(ctx.mode is Mode.Pattern) // expressions are handled separately by `binop`
          Apply(op, l :: r :: Nil) // op(l, r)
        }
      case PostfixOp(t, op) =>
        if ((ctx.mode is Mode.Type) && !op.isBackquoted && op.name == tpnme.raw.STAR) {
          val seqType = if (ctx.compilationUnit.isJava) defn.ArrayType else defn.SeqType
          Annotated(
            AppliedTypeTree(ref(seqType), t),
            New(ref(defn.RepeatedAnnotType), Nil :: Nil))
        } else {
          assert(ctx.mode.isExpr || ctx.reporter.hasErrors || ctx.mode.is(Mode.Interactive), ctx.mode)
          Select(t, op.name)
        }
      case PrefixOp(op, t) =>
        val nspace = if (ctx.mode.is(Mode.Type)) tpnme else nme
        Select(t, nspace.UNARY_PREFIX ++ op.name)
      case DoWhile(body, cond) =>
        // while ({ { body }; { cond } }) { () }
        // the inner blocks are there to protect the scopes of body and cond from each other
        WhileDo(Block(Block(Nil, body), Block(Nil, cond)), Literal(Constant(())))
      case ForDo(enums, body) =>
        makeFor(nme.foreach, nme.foreach, enums, body) orElse tree
      case ForYield(enums, body) =>
        makeFor(nme.map, nme.flatMap, enums, body) orElse tree
      case PatDef(mods, pats, tpt, rhs) =>
        val pats1 = if (tpt.isEmpty) pats else pats map (Typed(_, tpt))
        flatTree(pats1 map (makePatDef(tree, mods, _, rhs)))
      case ParsedTry(body, handler, finalizer) =>
        handler match {
          case Match(EmptyTree, cases) => Try(body, cases, finalizer)
          case EmptyTree => Try(body, Nil, finalizer)
          case _ =>
            Try(body,
              List(CaseDef(Ident(nme.DEFAULT_EXCEPTION_NAME), EmptyTree, Apply(handler, Ident(nme.DEFAULT_EXCEPTION_NAME)))),
              finalizer)
        }
    }
    desugared.withPosOf(tree)
  }

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
  def refinedTypeToClass(parent: tpd.Tree, refinements: List[Tree])(implicit ctx: Context): TypeDef = {
    def stripToCore(tp: Type): List[Type] = tp match {
      case tp: AppliedType => tp :: Nil
      case tp: TypeRef if tp.symbol.isClass => tp :: Nil     // monomorphic class type
      case tp: TypeProxy => stripToCore(tp.underlying)
      case AndType(tp1, tp2) => stripToCore(tp1) ::: stripToCore(tp2)
      case _ => defn.AnyType :: Nil
    }
    val parentCores = stripToCore(parent.tpe)
    val untpdParent = TypedSplice(parent)
    val (classParents, self) =
      if (parentCores.length == 1 && (parent.tpe eq parentCores.head)) (untpdParent :: Nil, EmptyValDef)
      else (parentCores map TypeTree, ValDef(nme.WILDCARD, untpdParent, EmptyTree))
    val impl = Template(emptyConstructor, classParents, self, refinements)
    TypeDef(tpnme.REFINE_CLASS, impl).withFlags(Trait)
  }

 /** If tree is of the form `id` or `id: T`, return its name and type, otherwise return None.
   */
  private object IdPattern {
    def unapply(tree: Tree)(implicit ctx: Context): Option[VarInfo] = tree match {
      case id: Ident => Some(id, TypeTree())
      case Typed(id: Ident, tpt) => Some((id, tpt))
      case _ => None
    }
  }

  /** Returns list of all pattern variables, possibly with their types,
   *  without duplicates
   */
  private def getVariables(tree: Tree)(implicit ctx: Context): List[VarInfo] = {
    val buf = new ListBuffer[VarInfo]
    def seenName(name: Name) = buf exists (_._1.name == name)
    def add(named: NameTree, t: Tree): Unit =
      if (!seenName(named.name)) buf += ((named, t))
    def collect(tree: Tree): Unit = tree match {
      case Bind(nme.WILDCARD, tree1) =>
        collect(tree1)
      case tree @ Bind(_, Typed(tree1, tpt)) if !mayBeTypePat(tpt) =>
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
        for (tree <- trees; (vble, _) <- getVariables(tree))
          ctx.error(IllegalVariableInPatternAlternative(), vble.sourcePos)
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
      case _ =>
    }
    collect(tree)
    buf.toList
  }

  private class IrrefutableGenFrom(pat: Tree, expr: Tree)(implicit @transientParam src: SourceInfo)
    extends GenFrom(pat, expr)
}
