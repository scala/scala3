package dotty.tools
package dotc
package ast

import core._
import util.Positions._, Types._, Contexts._, Constants._, Names._, NameOps._, Flags._
import SymDenotations._, Symbols._, StdNames._, Annotations._, Trees._
import Decorators._
import language.higherKinds
import collection.mutable.ListBuffer
import config.Printers._

object desugar {

  /** Are we using the new unboxed pair scheme? */
  private final val unboxedPairs = false

  import untpd._

  /** Info of a variable in a pattern: The named tree and its type */
  private type VarInfo = (NameTree, Tree)

  /** Names of methods that are added unconditionally to case classes */
  def isDesugaredCaseClassMethodName(name: Name)(implicit ctx: Context) =
    name == nme.isDefined ||
    name == nme.copy ||
    name == nme.productArity ||
    name.isSelectorName

// ----- DerivedTypeTrees -----------------------------------

  class SetterParamTree extends DerivedTypeTree {
    def derivedType(sym: Symbol)(implicit ctx: Context) = sym.info.resultType
  }

  class TypeRefTree extends DerivedTypeTree {
    def derivedType(sym: Symbol)(implicit ctx: Context) = sym.typeRef
  }

  class DerivedFromParamTree extends DerivedTypeTree {

    /** Make sure that for all enclosing module classes their companion lasses
     *  are completed. Reason: We need the constructor of such companion classes to
     *  be completed so that OriginalSymbol attachments are pushed to DerivedTypeTrees
     *  in apply/unapply methods.
     */
    override def ensureCompletions(implicit ctx: Context) =
      if (!(ctx.owner is Package))
        if (ctx.owner.isClass) {
          ctx.owner.ensureCompleted()
          if (ctx.owner is ModuleClass)
            ctx.owner.linkedClass.ensureCompleted()
        }
        else ensureCompletions(ctx.outer)

    /** Return info of original symbol, where all references to siblings of the
     *  original symbol (i.e. sibling and original symbol have the same owner)
     *  are rewired to same-named parameters or accessors in the scope enclosing
     *  the current scope. The current scope is the scope owned by the defined symbol
     *  itself, that's why we have to look one scope further out. If the resulting
     *  type is an alias type, dealias it. This is necessary because the
     *  accessor of a type parameter is a private type alias that cannot be accessed
     *  from subclasses.
     */
    def derivedType(sym: Symbol)(implicit ctx: Context) = {
      val relocate = new TypeMap {
        val originalOwner = sym.owner
        def apply(tp: Type) = tp match {
          case tp: NamedType if tp.symbol.exists && (tp.symbol.owner eq originalOwner) =>
            val defctx = ctx.outersIterator.dropWhile(_.scope eq ctx.scope).next
            var local = defctx.denotNamed(tp.name).suchThat(_ is ParamOrAccessor).symbol
            if (local.exists) (defctx.owner.thisType select local).dealias
            else throw new Error(s"no matching symbol for ${tp.symbol.showLocated} in ${defctx.owner} / ${defctx.effectiveScope}")
          case _ =>
            mapOver(tp)
        }
      }
      relocate(sym.info)
    }
  }

  /** A type definition copied from `tdef` with a rhs typetree derived from it */
  def derivedTypeParam(tdef: TypeDef) =
    cpy.TypeDef(tdef)(
      rhs = new DerivedFromParamTree() withPos tdef.rhs.pos watching tdef)

  /** A value definition copied from `vdef` with a tpt typetree derived from it */
  def derivedTermParam(vdef: ValDef) =
    cpy.ValDef(vdef)(
      tpt = new DerivedFromParamTree() withPos vdef.tpt.pos watching vdef)

// ----- Desugar methods -------------------------------------------------

  /**   var x: Int = expr
   *  ==>
   *    def x: Int = expr
   *    def x_=($1: <TypeTree()>): Unit = ()
   */
  def valDef(vdef: ValDef)(implicit ctx: Context): Tree = {
    val ValDef(name, tpt, rhs) = vdef
    val mods = vdef.mods
    def setterNeeded =
      (mods is Mutable) && ctx.owner.isClass && (!(mods is PrivateLocal) || (ctx.owner is Trait))
    if (setterNeeded) {
      // todo: copy of vdef as getter needed?
      // val getter = ValDef(mods, name, tpt, rhs) withPos vdef.pos ?
      // right now vdef maps via expandedTree to a thicket which concerns itself.
      // I don't see a problem with that but if there is one we can avoid it by making a copy here.
      val setterParam = makeSyntheticParameter(tpt = (new SetterParamTree).watching(vdef))
      val setterRhs = if (vdef.rhs.isEmpty) EmptyTree else unitLiteral
      val setter = cpy.DefDef(vdef)(
        name = name.setterName,
        tparams = Nil,
        vparamss = (setterParam :: Nil) :: Nil,
        tpt = TypeTree(defn.UnitType),
        rhs = setterRhs
      ).withMods((mods | Accessor) &~ CaseAccessor) // rhs gets filled in later, when field is generated and getter has parameters
      Thicket(vdef, setter)
    }
    else vdef
  }

  /** Expand context bounds to evidence params. E.g.,
   *
   *      def f[T >: L <: H : B](params)
   *  ==>
   *      def f[T >: L <: H](params)(implicit evidence$0: B[T])
   *
   *  Expand default arguments to default getters. E.g,
   *
   *      def f[T: B](x: Int = 1)(y: String = x + "m") = ...
   *  ==>
   *      def f[T](x: Int)(y: String)(implicit evidence$0: B[T]) = ...
   *      def f$default$1[T] = 1
   *      def f$default$2[T](x: Int) = x + "m"
   */
  def defDef(meth: DefDef, isPrimaryConstructor: Boolean = false)(implicit ctx: Context): Tree = {
    val DefDef(name, tparams, vparamss, tpt, rhs) = meth
    val mods = meth.mods
    val epbuf = new ListBuffer[ValDef]
    val tparams1 = tparams mapConserve {
      case tparam @ TypeDef(_, ContextBounds(tbounds, cxbounds)) =>
        for (cxbound <- cxbounds) {
          val paramFlags: FlagSet = if (isPrimaryConstructor) PrivateLocalParamAccessor else Param
          val epname = ctx.freshName(nme.EVIDENCE_PARAM_PREFIX).toTermName
          epbuf += ValDef(epname, cxbound, EmptyTree).withFlags(paramFlags | Implicit)
        }
        cpy.TypeDef(tparam)(rhs = tbounds)
      case tparam =>
        tparam
    }

    val meth1 = addEvidenceParams(cpy.DefDef(meth)(tparams = tparams1), epbuf.toList)

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
            name = meth.name.defaultGetterName(n),
            tparams = meth.tparams.map(tparam => dropContextBound(toDefParam(tparam))),
            vparamss = takeUpTo(normalizedVparamss, n),
            tpt = TypeTree(),
            rhs = vparam.rhs
          ).withMods(Modifiers(mods.flags & AccessFlags, mods.privateWithin))
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
            ((vparams ++ evidenceParams) :: rvparamss).reverse
          case _ =>
            meth.vparamss :+ evidenceParams
        }
        cpy.DefDef(meth)(vparamss = vparamss1)
    }

  /** The implicit evidence parameters of `meth`, as generated by `desugar.defDef` */
  private def evidenceParams(meth: DefDef)(implicit ctx: Context): List[ValDef] =
    meth.vparamss.reverse match {
      case (vparams @ (vparam :: _)) :: _ if vparam.mods is Implicit =>
        vparams.dropWhile(!_.name.startsWith(nme.EVIDENCE_PARAM_PREFIX))
      case _ =>
        Nil
    }

  /** Fill in empty type bounds with Nothing/Any. Expand private local type parameters as follows:
   *
   *     class C[v T]
   * ==>
   *     class C { type v C$T; type v T = C$T }
   */
  def typeDef(tdef: TypeDef)(implicit ctx: Context): Tree = {
    if (tdef.mods is PrivateLocalParam) {
      val tparam = cpy.TypeDef(tdef)(name = tdef.name.expandedName(ctx.owner))
        .withMods(tdef.mods &~ PrivateLocal | ExpandedName)
      val alias = cpy.TypeDef(tdef)(rhs = refOfDef(tparam), tparams = Nil)
        .withMods(tdef.mods & VarianceFlags | PrivateLocalParamAccessor | Synthetic)
      Thicket(tparam, alias)
    }
    else tdef
  }

  @sharable private val synthetic = Modifiers(Synthetic)

  private def toDefParam(tparam: TypeDef): TypeDef =
    tparam.withMods(tparam.rawMods & EmptyFlags | Param)
  private def toDefParam(vparam: ValDef): ValDef =
    vparam.withMods(vparam.rawMods & Implicit | Param)

  /** The expansion of a class definition. See inline comments for what is involved */
  def classDef(cdef: TypeDef)(implicit ctx: Context): Tree = {
    val TypeDef(name, impl @ Template(constr0, parents, self, _)) = cdef
    val mods = cdef.mods
    val companionMods = mods.withFlags((mods.flags & AccessFlags).toCommonFlags)

    val (constr1, defaultGetters) = defDef(constr0, isPrimaryConstructor = true) match {
      case meth: DefDef => (meth, Nil)
      case Thicket((meth: DefDef) :: defaults) => (meth, defaults)
    }

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

    val isCaseClass = mods.is(Case) && !mods.is(Module)
    val isValueClass = parents.nonEmpty && isAnyVal(parents.head)
      // This is not watertight, but `extends AnyVal` will be replaced by `inline` later.

    val constrTparams = constr1.tparams map toDefParam
    val constrVparamss =
      if (constr1.vparamss.isEmpty) { // ensure parameter list is non-empty
        if (isCaseClass)
          ctx.error("case class needs to have at least one parameter list", cdef.pos)
        ListOfNil
      }
      else constr1.vparamss.nestedMap(toDefParam)
    val constr = cpy.DefDef(constr1)(tparams = constrTparams, vparamss = constrVparamss)

    // Add constructor type parameters and evidence implicit parameters
    // to auxiliary constructors
    val normalizedBody = impl.body map {
      case ddef: DefDef if ddef.name.isConstructorName =>
        addEvidenceParams(
          cpy.DefDef(ddef)(tparams = constrTparams),
          evidenceParams(constr1).map(toDefParam))
      case stat =>
        stat
    }

    val derivedTparams = constrTparams map derivedTypeParam
    val derivedVparamss = constrVparamss nestedMap derivedTermParam
    val arity = constrVparamss.head.length

    var classTycon: Tree = EmptyTree

    // a reference to the class type, with all parameters given.
    val classTypeRef/*: Tree*/ = {
        // -language:keepUnions difference: classTypeRef needs type annotation, otherwise
        // infers Ident | AppliedTypeTree, which
        // renders the :\ in companions below untypable.
      classTycon = (new TypeRefTree) withPos cdef.pos.startPos // watching is set at end of method
      val tparams = impl.constr.tparams
      if (tparams.isEmpty) classTycon else AppliedTypeTree(classTycon, tparams map refOfDef)
    }

    // new C[Ts](paramss)
    lazy val creatorExpr = New(classTypeRef, constrVparamss nestedMap refOfDef)

    // Methods to add to a case class C[..](p1: T1, ..., pN: Tn)(moreParams)
    //     def isDefined = true
    //     def productArity = N
    //     def _1 = this.p1
    //     ...
    //     def _N = this.pN
    //     def copy(p1: T1 = p1: @uncheckedVariance, ...,
    //              pN: TN = pN: @uncheckedVariance)(moreParams) =
    //       new C[...](p1, ..., pN)(moreParams)
    //
    // Note: copy default parameters need @uncheckedVariance; see
    // neg/t1843-variances.scala for a test case. The test would give
    // two errors without @uncheckedVariance, one of them spurious.
    val caseClassMeths =
      if (isCaseClass) {
        def syntheticProperty(name: TermName, rhs: Tree) =
          DefDef(name, Nil, Nil, TypeTree(), rhs).withMods(synthetic)
        val isDefinedMeth = syntheticProperty(nme.isDefined, Literal(Constant(true)))
        val caseParams = constrVparamss.head.toArray
        val productElemMeths = for (i <- 0 until arity) yield
          syntheticProperty(nme.selectorName(i), Select(This(EmptyTypeName), caseParams(i).name))
        def isRepeated(tree: Tree): Boolean = tree match {
          case PostfixOp(_, nme.raw.STAR) => true
          case ByNameTypeTree(tree1) => isRepeated(tree1)
          case _ => false
        }
        val hasRepeatedParam = constrVparamss.exists(_.exists {
          case ValDef(_, tpt, _) => isRepeated(tpt)
          case _ => false
        })

        val copyMeths =
          if (mods.is(Abstract) || hasRepeatedParam) Nil  // cannot have default arguments for repeated parameters, hence copy method is not issued
          else {
            def copyDefault(vparam: ValDef) =
              makeAnnotated(defn.UncheckedVarianceAnnot, refOfDef(vparam))
            val copyFirstParams = derivedVparamss.head.map(vparam =>
              cpy.ValDef(vparam)(rhs = copyDefault(vparam)))
            val copyRestParamss = derivedVparamss.tail.nestedMap(vparam =>
              cpy.ValDef(vparam)(rhs = EmptyTree))
            DefDef(nme.copy, derivedTparams, copyFirstParams :: copyRestParamss, TypeTree(), creatorExpr)
              .withMods(synthetic) :: Nil
          }
        copyMeths ::: isDefinedMeth :: productElemMeths.toList
      }
      else Nil

    def anyRef = ref(defn.AnyRefAlias.typeRef)
    def productConstr(n: Int) = {
      val tycon = scalaDot((tpnme.Product.toString + n).toTypeName)
      val targs = constrVparamss.head map (_.tpt)
      if (targs.isEmpty) tycon else AppliedTypeTree(tycon, targs)
    }

    // Case classes and case objects get a ProductN parent
    var parents1 = parents
    if (mods.is(Case) && arity <= Definitions.MaxTupleArity)
      parents1 = parents1 :+ productConstr(arity)

    // The thicket which is the desugared version of the companion object
    //     synthetic object C extends parentTpt { defs }
    def companionDefs(parentTpt: Tree, defs: List[Tree]) =
      moduleDef(
        ModuleDef(
          name.toTermName, Template(emptyConstructor, parentTpt :: Nil, EmptyValDef, defs))
            .withMods(companionMods | Synthetic))
      .withPos(cdef.pos).toList

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
        val parent =
          if (constrTparams.nonEmpty ||
              constrVparamss.length > 1 ||
              mods.is(Abstract) ||
              constr.mods.is(Private)) anyRef
            // todo: also use anyRef if constructor has a dependent method type (or rule that out)!
          else (constrVparamss :\ classTypeRef) ((vparams, restpe) => Function(vparams map (_.tpt), restpe))
        val applyMeths =
          if (mods is Abstract) Nil
          else
            DefDef(nme.apply, derivedTparams, derivedVparamss, TypeTree(), creatorExpr)
              .withFlags(Synthetic | (constr1.mods.flags & DefaultParameterized)) :: Nil
        val unapplyMeth = {
          val unapplyParam = makeSyntheticParameter(tpt = classTypeRef)
          val unapplyRHS = if (arity == 0) Literal(Constant(true)) else Ident(unapplyParam.name)
          DefDef(nme.unapply, derivedTparams, (unapplyParam :: Nil) :: Nil, TypeTree(), unapplyRHS)
            .withMods(synthetic)
        }
        companionDefs(parent, applyMeths ::: unapplyMeth :: defaultGetters)
      }
      else if (defaultGetters.nonEmpty)
        companionDefs(anyRef, defaultGetters)
      else if (isValueClass)
        companionDefs(anyRef, Nil)
      else Nil


    // For an implicit class C[Ts](p11: T11, ..., p1N: T1N) ... (pM1: TM1, .., pMN: TMN), the method
    //     synthetic implicit C[Ts](p11: T11, ..., p1N: T1N) ... (pM1: TM1, ..., pMN: TMN): C[Ts] =
    //       new C[Ts](p11, ..., p1N) ... (pM1, ..., pMN) =
    val implicitWrappers =
      if (!mods.is(Implicit))
        Nil
      else if (ctx.owner is Package) {
        ctx.error("implicit classes may not be toplevel", cdef.pos)
        Nil
      }
      else if (isCaseClass) {
        ctx.error("implicit classes may not be case classes", cdef.pos)
        Nil
      }
      else
        // implicit wrapper is typechecked in same scope as constructor, so
        // we can reuse the constructor parameters; no derived params are needed.
        DefDef(name.toTermName, constrTparams, constrVparamss, classTypeRef, creatorExpr)
          .withMods(companionMods | Synthetic | Implicit)
          .withPos(cdef.pos) :: Nil

    val self1 = {
      val selfType = if (self.tpt.isEmpty) classTypeRef else self.tpt
      if (self.isEmpty) self
      else cpy.ValDef(self)(tpt = selfType).withMods(self.mods | SelfName)
    }

    val cdef1 = {
      val originalTparams = constr1.tparams.toIterator
      val originalVparams = constr1.vparamss.toIterator.flatten
      val tparamAccessors = derivedTparams.map(_.withMods(originalTparams.next.mods))
      val caseAccessor = if (isCaseClass) CaseAccessor else EmptyFlags
      val vparamAccessors = derivedVparamss.flatten.map(_.withMods(originalVparams.next.mods | caseAccessor))
      cpy.TypeDef(cdef)(
        rhs = cpy.Template(impl)(constr, parents1, self1,
          tparamAccessors ::: vparamAccessors ::: normalizedBody ::: caseClassMeths),
        tparams = Nil)
    }

    // install the watch on classTycon
    classTycon match {
      case tycon: DerivedTypeTree => tycon.watching(cdef1)
      case _ =>
    }

    flatTree(cdef1 :: companions ::: implicitWrappers)
  }

  val AccessOrSynthetic = AccessFlags | Synthetic

  /** Expand
   *
   *    object name extends parents { self => body }
   *
   *  to:
   *    <module> val name: name$ = New(name$)
   *    <module> final class name$ extends parents { self: name.type => body }
   */
  def moduleDef(mdef: ModuleDef)(implicit ctx: Context): Tree = {
    val ModuleDef(name, tmpl) = mdef
    val mods = mdef.mods
    if (mods is Package)
      PackageDef(Ident(name), cpy.ModuleDef(mdef)(nme.PACKAGE, tmpl).withMods(mods &~ Package) :: Nil)
    else {
      val clsName = name.moduleClassName
      val clsRef = Ident(clsName)
      val modul = ValDef(name, clsRef, New(clsRef, Nil))
        .withMods(mods | ModuleCreationFlags | mods.flags & AccessFlags)
        .withPos(mdef.pos)
      val ValDef(selfName, selfTpt, _) = tmpl.self
      val selfMods = tmpl.self.mods
      if (!selfTpt.isEmpty) ctx.error("object definition may not have a self type", tmpl.self.pos)
      val clsSelf = ValDef(selfName, SingletonTypeTree(Ident(name)), tmpl.self.rhs)
        .withMods(selfMods)
        .withPos(tmpl.self.pos orElse tmpl.pos.startPos)
      val clsTmpl = cpy.Template(tmpl)(self = clsSelf, body = tmpl.body)
      val cls = TypeDef(clsName, clsTmpl)
        .withMods(mods.toTypeFlags & RetainedModuleClassFlags | ModuleClassCreationFlags)
      Thicket(modul, classDef(cls))
    }
  }

  /**     val p1, ..., pN: T = E
   *  ==>
   *      makePatDef[[val p1: T1 = E]]; ...; makePatDef[[val pN: TN = E]]
   */
  def patDef(pdef: PatDef)(implicit ctx: Context): Tree = {
    val PatDef(mods, pats, tpt, rhs) = pdef
    val pats1 = if (tpt.isEmpty) pats else pats map (Typed(_, tpt))
    flatTree(pats1 map (makePatDef(mods, _, rhs)))
  }

  /** If `pat` is a variable pattern,
   *
   *    val/var/lazy val p = e
   *
   *  Otherwise, in case there is exactly one variable x_1 in pattern
   *   val/var/lazy val p = e  ==>  val/var/lazy val x_1 = (e: @unchecked) match (case p => (x_1))
   *
   *   in case there are zero or more than one variables in pattern
   *   val/var/lazy p = e  ==>  private synthetic [lazy] val t$ = (e: @unchecked) match (case p => (x_1, ..., x_N))
   *                   val/var/def x_1 = t$._1
   *                   ...
   *                   val/var/def x_N = t$._N
   *  If the original pattern variable carries a type annotation, so does the corresponding
   *  ValDef or DefDef.
   */
  def makePatDef(mods: Modifiers, pat: Tree, rhs: Tree)(implicit ctx: Context): Tree = pat match {
    case VarPattern(named, tpt) =>
      derivedValDef(named, tpt, rhs, mods)
    case _ =>
      val rhsUnchecked = makeAnnotated(defn.UncheckedAnnot, rhs)
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
          derivedValDef(named, tpt, matchExpr, mods)
        case _ =>
          val tmpName = ctx.freshName().toTermName
          val patMods = mods & (AccessFlags | Lazy) | Synthetic
          val firstDef =
            ValDef(tmpName, TypeTree(), matchExpr)
              .withPos(pat.pos.union(rhs.pos)).withMods(patMods)
          def selector(n: Int) = Select(Ident(tmpName), nme.selectorName(n))
          val restDefs =
            for (((named, tpt), n) <- vars.zipWithIndex)
            yield
              if (mods is Lazy) derivedDefDef(named, tpt, selector(n), mods &~ Lazy)
              else derivedValDef(named, tpt, selector(n), mods)
          flatTree(firstDef :: restDefs)
      }
  }

  /** Expand variable identifier x to x @ _ */
  def patternVar(tree: Tree)(implicit ctx: Context) = {
    val Ident(name) = tree
    Bind(name, Ident(nme.WILDCARD)).withPos(tree.pos)
  }

  def defTree(tree: Tree)(implicit ctx: Context): Tree = tree match {
    case tree: ValDef => valDef(tree)
    case tree: TypeDef => if (tree.isClassDef) classDef(tree) else typeDef(tree)
    case tree: DefDef => defDef(tree)
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
        unitLiteral withPos (if (tree.stats.isEmpty) tree.pos else tree.pos.endPos))
    case _ =>
      tree
  }

  /** EmptyTree in lower bound ==> Nothing
   *  EmptyTree in upper bounds ==> Any
   */
  def typeBoundsTree(tree: TypeBoundsTree)(implicit ctx: Context): TypeBoundsTree = {
    val TypeBoundsTree(lo, hi) = tree
    val lo1 = if (lo.isEmpty) untpd.TypeTree(defn.NothingType) else lo
    val hi1 = if (hi.isEmpty) untpd.TypeTree(defn.AnyType) else hi
    cpy.TypeBoundsTree(tree)(lo1, hi1)
  }

  /** Make closure corresponding to function.
   *      params => body
   *  ==>
   *      def $anonfun(params) = body
   *      Closure($anonfun)
   */
  def makeClosure(params: List[ValDef], body: Tree, tpt: Tree = TypeTree()) =
    Block(
      DefDef(nme.ANON_FUN, Nil, params :: Nil, tpt, body).withMods(synthetic),
      Closure(Nil, Ident(nme.ANON_FUN), EmptyTree))

  /** If `nparams` == 1, expand partial function
   *
   *       { cases }
   *  ==>
   *       x$1 => x$1 match { cases }
   *
   *  If `nparams` != 1, expand instead to
   *
   *       (x$1, ..., x$n) => (x$0, ..., x${n-1}) match { cases }
   */
  def makeCaseLambda(cases: List[CaseDef], nparams: Int = 1)(implicit ctx: Context) = {
    val params = (1 to nparams).toList.map(makeSyntheticParameter(_))
    val selector = makeTuple(params.map(p => Ident(p.name)))
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
   */
  def makeTupledFunction(params: List[ValDef], body: Tree)(implicit ctx: Context): Tree = {
    val param = makeSyntheticParameter()
    def selector(n: Int) = Select(refOfDef(param), nme.selectorName(n))
    val vdefs =
      params.zipWithIndex.map{
        case (param, idx) =>
          DefDef(param.name, Nil, Nil, TypeTree(), selector(idx)).withPos(param.pos)
      }
    Function(param :: Nil, Block(vdefs, body))
  }

  /** Add annotation with class `cls` to tree:
   *      tree @cls
   */
  def makeAnnotated(cls: Symbol, tree: Tree)(implicit ctx: Context) =
    Annotated(untpd.New(untpd.TypeTree(cls.typeRef), Nil), tree)

  private def derivedValDef(named: NameTree, tpt: Tree, rhs: Tree, mods: Modifiers)(implicit ctx: Context) = {
    val vdef = ValDef(named.name.asTermName, tpt, rhs).withMods(mods).withPos(named.pos)
    val mayNeedSetter = valDef(vdef)
    mayNeedSetter
   }

  private def derivedDefDef(named: NameTree, tpt: Tree, rhs: Tree, mods: Modifiers) =
    DefDef(named.name.asTermName, Nil, Nil, tpt, rhs).withMods(mods).withPos(named.pos)

  /** Main desugaring method */
  def apply(tree: Tree)(implicit ctx: Context): Tree = {

    /**    { label def lname(): Unit = rhs; call }
     */
    def labelDefAndCall(lname: TermName, rhs: Tree, call: Tree) = {
      val ldef = DefDef(lname, Nil, ListOfNil, TypeTree(defn.UnitType), rhs).withFlags(Label)
      Block(ldef, call)
    }

    /** Translate infix operation expression  left op right
     */
    def makeBinop(left: Tree, op: Name, right: Tree): Tree = {
      def assignToNamedArg(arg: Tree) = arg match {
        case Assign(Ident(name), rhs) => cpy.NamedArg(arg)(name, rhs)
        case _ => arg
      }
      if (isLeftAssoc(op)) {
        val args: List[Tree] = right match {
          case Parens(arg) => assignToNamedArg(arg) :: Nil
          case Tuple(args) => args mapConserve assignToNamedArg
          case _ => right :: Nil
        }
        Apply(Select(left, op), args)
      } else {
        val x = ctx.freshName().toTermName
        Block(
          ValDef(x, TypeTree(), left).withMods(synthetic),
          Apply(Select(right, op), Ident(x)))
      }
    }

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
    def makeFor(mapName: TermName, flatMapName: TermName, enums: List[Tree], body: Tree): Tree = ctx.traceIndented(i"make for ${ForYield(enums, body)}", show = true) {

      /** Make a function value pat => body.
       *  If pat is a var pattern id: T then this gives (id: T) => body
       *  Otherwise this gives { case pat => body }
       */
      def makeLambda(pat: Tree, body: Tree): Tree = pat match {
        case VarPattern(named, tpt) =>
          Function(derivedValDef(named, tpt, EmptyTree, Modifiers(Param)) :: Nil, body)
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
          val name = ctx.freshName().toTermName
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
        Apply(Select(rhs, nme.withFilter), Match(EmptyTree, cases))
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
        gen.isInstanceOf[IrrefutableGenFrom] || isIrrefutable(gen.pat, gen.expr)

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
        case (enum @ GenFrom(pat, rhs)) :: (rest @ GenAlias(_, _) :: _) =>
          val (valeqs, rest1) = rest.span(_.isInstanceOf[GenAlias])
          val pats = valeqs map { case GenAlias(pat, _) => pat }
          val rhss = valeqs map { case GenAlias(_, rhs) => rhs }
          val (defpat0, id0) = makeIdPat(pat)
          val (defpats, ids) = (pats map makeIdPat).unzip
          val pdefs = (defpats, rhss).zipped map (makePatDef(Modifiers(), _, _))
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
    tree match {
      case SymbolLit(str) =>
        Apply(
          ref(defn.SymbolClass.companionModule.termRef),
          Literal(Constant(str)) :: Nil)
      case InterpolatedString(id, strs, elems) =>
        Apply(Select(Apply(Ident(nme.StringContext), strs), id), elems)
      case InfixOp(l, op, r) =>
        if (ctx.mode is Mode.Type)
          if (op == tpnme.raw.AMP) AndTypeTree(l, r)     // l & r
          else if (op == tpnme.raw.BAR) OrTypeTree(l, r) // l | r
          else AppliedTypeTree(Ident(op), l :: r :: Nil) // op[l, r]
        else if (ctx.mode is Mode.Pattern)
          Apply(Ident(op), l :: r :: Nil) // op(l, r)
        else // l.op(r), or val x = r; l.op(x), plus handle named args specially
          makeBinop(l, op, r)
      case PostfixOp(t, op) =>
        if ((ctx.mode is Mode.Type) && op == nme.raw.STAR) {
          val seqType = if (ctx.compilationUnit.isJava) defn.ArrayType else defn.SeqType
          Annotated(
            New(ref(defn.RepeatedAnnotType), Nil :: Nil),
            AppliedTypeTree(ref(seqType), t))
        } else {
          assert(ctx.mode.isExpr || ctx.reporter.hasErrors, ctx.mode)
          Select(t, op)
        }
      case PrefixOp(op, t) =>
        Select(t, nme.UNARY_PREFIX ++ op)
      case Parens(t) =>
        t
      case Tuple(ts) =>
        if (unboxedPairs) {
          def PairTypeTree(l: Tree, r: Tree) =
            AppliedTypeTree(ref(defn.PairType), l :: r :: Nil)
          if (ctx.mode is Mode.Type) ts.reduceRight(PairTypeTree)
          else if (ts.isEmpty) unitLiteral
          else ts.reduceRight(Pair(_, _))
        }
        else {
          val arity = ts.length
          def tupleTypeRef = defn.TupleType(arity)
          if (arity > Definitions.MaxTupleArity) {
            ctx.error(s"tuple too long (max allowed: ${Definitions.MaxTupleArity})", tree.pos)
            unitLiteral
          }
          else if (arity == 1) ts.head
          else if (ctx.mode is Mode.Type) AppliedTypeTree(ref(tupleTypeRef), ts)
          else if (arity == 0) unitLiteral
          else Apply(ref(tupleTypeRef.classSymbol.companionModule.valRef), ts)
        }
      case WhileDo(cond, body) =>
        // { <label> def while$(): Unit = if (cond) { body; while$() } ; while$() }
        val call = Apply(Ident(nme.WHILE_PREFIX), Nil)
        val rhs = If(cond, Block(body, call), unitLiteral)
        labelDefAndCall(nme.WHILE_PREFIX, rhs, call)
      case DoWhile(body, cond) =>
        // { label def doWhile$(): Unit = { body; if (cond) doWhile$() } ; doWhile$() }
        val call = Apply(Ident(nme.DO_WHILE_PREFIX), Nil)
        val rhs = Block(body, If(cond, call, unitLiteral))
        labelDefAndCall(nme.DO_WHILE_PREFIX, rhs, call)
      case ForDo(enums, body) =>
        makeFor(nme.foreach, nme.foreach, enums, body) orElse tree
      case ForYield(enums, body) =>
        makeFor(nme.map, nme.flatMap, enums, body) orElse tree
      case PatDef(mods, pats, tpt, rhs) =>
        val pats1 = if (tpt.isEmpty) pats else pats map (Typed(_, tpt))
        flatTree(pats1 map (makePatDef(mods, _, rhs)))
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
  }.withPos(tree.pos)

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
      case tp: RefinedType if tp.argInfos.nonEmpty => tp :: Nil // parameterized class type
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

 /** If tree is a variable pattern, return its name and type, otherwise return None.
   */
  private object VarPattern {
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
      case Bind(nme.WILDCARD, _) =>
        collect(tree)
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
      case Pair(left, right) =>
        collect(left)
        collect(right)
      case Typed(expr, _) =>
        collect(expr)
      case NamedArg(_, arg) =>
        collect(arg)
      case SeqLiteral(elems, _) =>
        elems foreach collect
      case Alternative(trees) =>
        for (tree <- trees; (vble, _) <- getVariables(tree))
          ctx.error("illegal variable in pattern alternative", vble.pos)
      case Annotated(annot, arg) =>
        collect(arg)
      case InterpolatedString(_, _, elems) =>
        elems foreach collect
      case InfixOp(left, _, right) =>
        collect(left)
        collect(right)
      case PrefixOp(_, od) =>
        collect(od)
      case Parens(tree) =>
        collect(tree)
      case Tuple(trees) =>
        trees foreach collect
      case _ =>
    }
    collect(tree)
    buf.toList
  }

  private class IrrefutableGenFrom(pat: Tree, expr: Tree) extends GenFrom(pat, expr)
}
