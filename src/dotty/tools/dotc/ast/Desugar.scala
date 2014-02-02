package dotty.tools
package dotc
package ast

import core._
import util.Positions._, Types._, Contexts._, Constants._, Names._, NameOps._, Flags._
import SymDenotations._, Symbols._, StdNames._, Annotations._, Trees._
import Decorators._
import language.higherKinds
import collection.mutable.ListBuffer
import typer.ErrorReporting.InfoString
import typer.Mode

object desugar {

  /** Are we using the new unboxed pair scheme? */
  private final val unboxedPairs = false

  import untpd._

  /** Info of a variable in a pattern: The named tree and its type */
  private type VarInfo = (NameTree, Tree)

  /**   var x: Int = expr
   *  ==>
   *    def x: Int = expr
   *    def x_=($1: <TypeTree()>): Unit = ()
   */
  def valDef(vdef: ValDef)(implicit ctx: Context): Tree = {
    val ValDef(mods, name, tpt, rhs) = vdef
    def setterNeeded =
      (mods is Mutable) && ctx.owner.isClass && (!(mods is Private) || (ctx.owner is Trait))
    if (setterNeeded) {
      // todo: copy of vdef as getter needed?
      // val getter = ValDef(mods, name, tpt, rhs) withPos vdef.pos ?
      // right now vdef maps via expandedTree to a thicket which concerns itself.
      // I don't see a problem with that but if there is one we can avoid it by making a copy here.
      val setterParam = makeSyntheticParameter(tpt = TypeTree())
      val setterRhs = if (vdef.rhs.isEmpty) EmptyTree else unitLiteral
      val setter = cpy.DefDef(vdef,
        mods | Accessor, name.setterName, Nil, (setterParam :: Nil) :: Nil,
        TypeTree(defn.UnitType), setterRhs) // rhs gets filled in later, when field is generated and getter has parameters
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
   *      def f(x: Int = 1)(y: String = x + "m") = ...
   *  ==>
   *      def f(x: Int)(y: String) = ...
   *      def f$default$1 = 1
   *      def f$default$2(x: Int) = x + "m"
   */
  def defDef(meth: DefDef, isPrimaryConstructor: Boolean = false)(implicit ctx: Context): Tree = {
    val DefDef(mods, name, tparams, vparamss, tpt, rhs) = meth
    val epbuf = new ListBuffer[ValDef]
    val tparams1 = tparams mapConserve {
      case tparam @ TypeDef(mods, name, ContextBounds(tbounds, cxbounds)) =>
        for (cxbound <- cxbounds) {
          val paramFlags: FlagSet = if (isPrimaryConstructor) PrivateLocalParamAccessor else Param
          val epname = (nme.EVIDENCE_PARAM_PREFIX.toString + epbuf.length).toTermName
          epbuf +=
            ValDef(Modifiers(paramFlags | Implicit), epname, cxbound, EmptyTree)
        }
        cpy.TypeDef(tparam, mods, name, tbounds, tparam.tparams)
      case tparam =>
        tparam
    }

    val meth1 = epbuf.toList match {
      case Nil =>
        meth
      case evidenceParams =>
        val vparamss1 = vparamss.reverse match {
          case (vparams @ (vparam :: _)) :: rvparamss if vparam.mods is Implicit =>
            ((vparams ++ evidenceParams) :: rvparamss).reverse
          case _ =>
            vparamss :+ evidenceParams
        }
        cpy.DefDef(meth, mods, name, tparams1, vparamss1, tpt, rhs)
    }

    /** The longest prefix of parameter lists in vparamss whose total length does not exceed `n` */
    def takeUpTo(vparamss: List[List[ValDef]], n: Int): List[List[ValDef]] = vparamss match {
      case vparams :: vparamss1 =>
        val len = vparams.length
        if (n >= len) vparams :: takeUpTo(vparamss1, n - len) else Nil
      case _ =>
        Nil
    }

    def normalizedVparamss = vparamss map (_ map (vparam =>
      cpy.ValDef(vparam, vparam.mods, vparam.name, vparam.tpt, EmptyTree)))

    def defaultGetters(vparamss: List[List[ValDef]], n: Int): List[DefDef] = vparamss match {
      case (vparam :: vparams) :: vparamss1 =>
        def defaultGetter: DefDef =
          DefDef(
            mods = vparam.mods & AccessFlags,
            name = meth.name.defaultGetterName(n),
            tparams = meth.tparams map toDefParam,
            vparamss = takeUpTo(normalizedVparamss, n),
            tpt = TypeTree(),
            rhs = vparam.rhs)
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
      val mods1 = meth1.mods | DefaultParameterized
      val meth2 = cpy.DefDef(meth1, meth1.mods | DefaultParameterized,
          meth1.name, meth1.tparams, normalizedVparamss, meth1.tpt, meth1.rhs)
      Thicket(meth2 :: defGetters)
    }
  }

  /** Fill in empty type bounds with Nothing/Any. Expand private local type parameters as follows:
   *
   *     class C[T]
   * ==>
   *     class C { type C$T; type T = C$T }
   */
  def typeDef(tdef: TypeDef)(implicit ctx: Context): Tree = {
    val TypeDef(mods, name, rhs) = tdef
    if (mods is PrivateLocalParam) {
      val tparam = cpy.TypeDef(tdef,
        mods &~ PrivateLocal | ExpandedName, name.expandedName(ctx.owner), rhs, tdef.tparams)
      val alias = cpy.TypeDef(tdef,
        Modifiers(PrivateLocalParamAccessor | Synthetic), name, refOfDef(tparam))
      Thicket(tparam, alias)
    }
    else cpy.TypeDef(tdef, mods, name, rhs, tdef.tparams)
  }

  private val synthetic = Modifiers(Synthetic)

  private def toDefParam(tparam: TypeDef) =
    cpy.TypeDef(tparam, Modifiers(Param), tparam.name, tparam.rhs, tparam.tparams)

  /** The expansion of a class definition. See inline comments for what is involved */
  def classDef(cdef: TypeDef)(implicit ctx: Context): Tree = {
    val TypeDef(
      mods, name, impl @ Template(constr0, parents, self, body)) = cdef

    val (constr1, defaultGetters) = defDef(constr0, isPrimaryConstructor = true) match {
      case meth: DefDef => (meth, Nil)
      case Thicket((meth: DefDef) :: defaults) => (meth, defaults)
    }

    // The original type and value parameters in the constructor already have the flags
    // needed to be type members (i.e. param, and possibly also private and local unless
    // prefixed by type or val). `tparams` and `vparamss` are the type parameters that
    // go in `constr`, the constructor after desugaring.

    val tparams = constr1.tparams map toDefParam
    val vparamss =
      if (constr1.vparamss.isEmpty) { // ensure parameter list is non-empty
        if (mods is Case)
          ctx.error("case class needs to have at least one parameter list", cdef.pos)
        ListOfNil
      } else
        constr1.vparamss.nestedMap(vparam => cpy.ValDef(vparam,
          Modifiers(Param | vparam.mods.flags & Implicit), vparam.name, vparam.tpt, vparam.rhs))

    val constr = cpy.DefDef(constr1,
      constr1.mods, constr1.name, tparams, vparamss, constr1.tpt, constr1.rhs)

    // a reference to the class type, with all parameters given.
    val classTypeRef: Tree = {
        // Dotty deviation: Without type annotation infers Ident | AppliedTypeTree, which
        // renders the :\ in companions below untypable.
      val tycon = Ident(cdef.name) withPos cdef.pos.startPos
      val tparams = impl.constr.tparams
      if (tparams.isEmpty) tycon else AppliedTypeTree(tycon, tparams map refOfDef)
    }

    // new C[Ts](paramss)
    lazy val creatorExpr = New(classTypeRef, vparamss nestedMap refOfDef)

    // Methods to add to a case class C[..](p1: T1, ..., pN: Tn)(moreParams)
    //     def isDefined = true
    //     def productArity = N
    //     def _1 = this.p1
    //     ...
    //     def _N = this.pN
    //     def copy(p1: T1 = p1, ..., pN: TN = pN)(moreParams) = new C[...](p1, ..., pN)(moreParams)
    val caseClassMeths =
      if (mods is Case) {
        val caseParams = vparamss.head.toArray
        def syntheticProperty(name: TermName, rhs: Tree) =
          DefDef(synthetic, name, Nil, Nil, TypeTree(), rhs)
        val isDefinedMeth = syntheticProperty(nme.isDefined, Literal(Constant(true)))
        val productArityMeth = syntheticProperty(nme.productArity, Literal(Constant(caseParams.length)))
        def selectorName(n: Int) =
          if (caseParams.length == 1) nme.get else nme.selectorName(n)
        val productElemMeths = for (i <- 0 until caseParams.length) yield
          syntheticProperty(selectorName(i), Select(This(EmptyTypeName), caseParams(i).name))
        val copyMeths =
          if (mods is Abstract) Nil
          else {
            val copyFirstParams = vparamss.head.map(vparam =>
              cpy.ValDef(vparam, vparam.mods, vparam.name, vparam.tpt, refOfDef(vparam)))
            val copyRestParamss = vparamss.tail.nestedMap(vparam =>
              cpy.ValDef(vparam, vparam.mods, vparam.name, vparam.tpt, EmptyTree))
            DefDef(synthetic, nme.copy, tparams, copyFirstParams :: copyRestParamss, TypeTree(), creatorExpr) :: Nil
          }
        copyMeths ::: isDefinedMeth :: productArityMeth :: productElemMeths.toList
      }
      else Nil

    def anyRef = ref(defn.AnyRefAlias.typeRef)
    def productConstr(n: Int) = {
      val tycon = ref(defn.ProductNClass(n).typeRef)
      val targs = vparamss.head map (_.tpt)
      New(AppliedTypeTree(tycon, targs), Nil)
    }

    // Case classes get a ProductN parent
    var parents1 = parents
    val n = vparamss.head.length
    if ((mods is Case) && 2 <= n && n <= Definitions.MaxTupleArity)
      parents1 = parents1 :+ productConstr(n)

    // The thicket which is the desugared version of the companion object
    //     synthetic object C extends parentTpt { defs }
    def companionDefs(parentTpt: Tree, defs: List[Tree]) =
      moduleDef(
        ModuleDef(
          Modifiers(Synthetic), name.toTermName,
          Template(emptyConstructor, New(parentTpt, Nil) :: Nil, EmptyValDef, defs))).toList

    // The companion object defifinitions, if a companion is needed, Nil otherwise.
    // companion definitions include:
    // 1. If class is a case class case class C[Ts](p1: T1, ..., pN: TN)(moreParams):
    //     def apply[Ts](p1: T1, ..., pN: TN)(moreParams) = new C[Ts](p1, ..., pN)(moreParams)  (unless C is abstract)
    //     def unapply[Ts]($1: C[Ts]) = $1
    // 2. The default getters of the constructor
    // The parent of the companion object of a non-parameterized case class
    //     (T11, ..., T1N) => ... => (TM1, ..., TMN) => C
    // For all other classes, the parent is AnyRef.
    val companions =
      if (mods is Case) {
        val parent =
          if (tparams.nonEmpty) anyRef
          else (vparamss :\ classTypeRef) ((vparams, restpe) => Function(vparams map (_.tpt), restpe))
        val applyMeths =
          if (mods is Abstract) Nil
          else DefDef(
              synthetic | (constr1.mods.flags & DefaultParameterized), nme.apply,
              tparams, vparamss, TypeTree(), creatorExpr) :: Nil
        val unapplyMeth = {
          val unapplyParam = makeSyntheticParameter(tpt = classTypeRef)
          val unapplyRHS = if (n == 0) Literal(Constant(true)) else Ident(unapplyParam.name)
          DefDef(synthetic, nme.unapply, tparams, (unapplyParam :: Nil) :: Nil, TypeTree(), unapplyRHS)
        }
        companionDefs(parent, applyMeths ::: unapplyMeth :: defaultGetters)
      }
      else if (defaultGetters.nonEmpty)
        companionDefs(anyRef, defaultGetters)
      else Nil

    // For an implicit class C[Ts](p11: T11, ..., p1N: T1N) ... (pM1: TM1, .., pMN: TMN), the method
    //     synthetic implicit C[Ts](p11: T11, ..., p1N: T1N) ... (pM1: TM1, ..., pMN: TMN): C[Ts] =
    //       new C[Ts](p11, ..., p1N) ... (pM1, ..., pMN) =
    val implicitWrappers =
      if (mods is Implicit) {
        if (ctx.owner is Package)
          ctx.error("implicit classes may not be toplevel", cdef.pos)
        if (mods is Case)
          ctx.error("implicit classes may not case classes", cdef.pos)
        DefDef(Modifiers(Synthetic | Implicit), name.toTermName,
          tparams, vparamss, classTypeRef, creatorExpr) :: Nil
      }
      else Nil

    val selfType = if (self.tpt.isEmpty) classTypeRef else self.tpt
    val self1 =
      if (self.isEmpty) self
      else cpy.ValDef(self, self.mods | SelfName, self.name, selfType, self.rhs)

    val cdef1 = cpy.TypeDef(cdef, mods, name,
      cpy.Template(impl, constr, parents1, self1,
        constr1.tparams ::: constr1.vparamss.flatten ::: body ::: caseClassMeths))
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
    val ModuleDef(mods, name, tmpl @ Template(constr, parents, self, body)) = mdef
    if (mods is Package)
      PackageDef(Ident(name), cpy.ModuleDef(mdef, mods &~ Package, nme.PACKAGE, tmpl) :: Nil)
    else {
      val clsName = name.moduleClassName
      val clsRef = Ident(clsName)
      val modul = ValDef(mods | ModuleCreationFlags, name, clsRef, New(clsRef, Nil)) withPos mdef.pos
      val ValDef(selfMods, selfName, selfTpt, selfRhs) = self
      if (!selfTpt.isEmpty) ctx.error("object definition may not have a self type", self.pos)
      val clsSelf = ValDef(selfMods, selfName, SingletonTypeTree(Ident(name)), selfRhs)
        .withPos(self.pos orElse tmpl.pos.startPos)
      val clsTmpl = cpy.Template(tmpl, constr, parents, clsSelf, body)
      val cls = TypeDef(mods.toTypeFlags & AccessFlags | ModuleClassCreationFlags, clsName, clsTmpl)
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
   *    val/var p = e
   *
   *  Otherwise, in case there is exactly one variable x_1 in pattern
   *   val/var p = e  ==>  val/var x_1 = (e: @unchecked) match (case p => (x_1))
   *
   *   in case there are zero or more than one variables in pattern
   *   val/var p = e  ==>  private synthetic val t$ = (e: @unchecked) match (case p => (x_1, ..., x_N))
   *                   val/var x_1 = t$._1
   *                   ...
   *                  val/var x_N = t$._N
   *  If the original pattern variable carries a type annotation, so does the corresponding
   *  ValDef.
   */
  def makePatDef(mods: Modifiers, pat: Tree, rhs: Tree)(implicit ctx: Context): Tree = pat match {
    case VarPattern(named, tpt) =>
      derivedValDef(mods, named, tpt, rhs)
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
          derivedValDef(mods, named, tpt, matchExpr)
        case _ =>
          val tmpName = ctx.freshName().toTermName
          val patMods = Modifiers(PrivateLocal | Synthetic | (mods.flags & Lazy))
          val firstDef = ValDef(patMods, tmpName, TypeTree(), matchExpr)
          def selector(n: Int) = Select(Ident(tmpName), nme.selectorName(n))
          val restDefs =
            for (((named, tpt), n) <- vars.zipWithIndex)
              yield derivedValDef(mods, named, tpt, selector(n))
          flatTree(firstDef :: restDefs)
      }
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
      cpy.Block(tree, tree.stats,
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
    cpy.TypeBoundsTree(tree, lo1, hi1)
  }

  /** Make closure corresponding to function.
   *      params => body
   *  ==>
   *      def $anonfun(params) = body
   *      Closure($anonfun)
   */
  def makeClosure(params: List[ValDef], body: Tree, tpt: Tree = TypeTree()) =
    Block(
      DefDef(Modifiers(Synthetic), nme.ANON_FUN, Nil, params :: Nil, tpt, body),
      Closure(Nil, Ident(nme.ANON_FUN), EmptyTree))

  /** Expand partial function
   *       { cases }
   *  ==>
   *       x$0 => x$0 match { cases }
   */
  def makeCaseLambda(cases: List[CaseDef])(implicit ctx: Context) = {
    val param = makeSyntheticParameter()
    Function(param :: Nil, Match(Ident(param.name), cases))
  }

  /** Add annotation with class `cls` to tree:
   *      tree @cls
   */
  def makeAnnotated(cls: Symbol, tree: Tree)(implicit ctx: Context) =
    Annotated(TypedSplice(tpd.New(cls.typeRef, Nil)), tree)

  private def derivedValDef(mods: Modifiers, named: NameTree, tpt: Tree, rhs: Tree) =
    ValDef(mods, named.name.asTermName, tpt, rhs).withPos(named.pos)

  /** Main desugaring method */
  def apply(tree: Tree)(implicit ctx: Context): Tree = {

    /**    { label def lname(): Unit = rhs; call }
     */
    def labelDefAndCall(lname: TermName, rhs: Tree, call: Tree) = {
      val ldef = DefDef(Modifiers(Label), lname, Nil, ListOfNil, TypeTree(defn.UnitType), rhs)
      Block(ldef, call)
    }

    /** Translate infix operation expression  left op right
     */
    def makeBinop(left: Tree, op: Name, right: Tree): Tree = {
      def assignToNamedArg(arg: Tree) = arg match {
        case Assign(Ident(name), rhs) => cpy.NamedArg(arg, name, rhs)
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
          ValDef(Modifiers(Synthetic), x, TypeTree(), left),
          Apply(Select(right, op), Ident(x)))
      }
    }

    /** Create tree for for-comprehension <for (enums) do body> or
     *   <for (enums) yield body> where mapName and flatMapName are chosen
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
     *    If any of the P_i are variable patterns, the corresponding `x_i @ P_i' is not generated
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
          Function(derivedValDef(Modifiers(Param), named, tpt, EmptyTree) :: Nil, body)
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
          Select(ref(defn.SymbolClass.companionModule.termRef), nme.apply),
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
        if ((ctx.mode is Mode.Type) && op == nme.raw.STAR)
          AppliedTypeTree(ref(defn.RepeatedParamType), t)
        else {
          assert(ctx.mode.isExpr, ctx.mode)
          Select(t, op)
        }
      case PrefixOp(op, t) =>
        Select(t, nme.UNARY_PREFIX ++ op)
      case Parens(t) =>
        t
      case Tuple(ts) =>
        if (unboxedPairs) {
          def PairTypeTree(l: Tree, r: Tree) =
            AppliedTypeTree(ref(defn.PairClass.typeRef), l :: r :: Nil)
          if (ctx.mode is Mode.Type) ts.reduceRight(PairTypeTree)
          else if (ts.isEmpty) unitLiteral
          else ts.reduceRight(Pair(_, _))
        }
        else {
          val arity = ts.length
          def tupleClass = defn.TupleClass(arity)
          if (arity > Definitions.MaxTupleArity) {
            ctx.error(s"tuple too long (max allowed: ${Definitions.MaxTupleArity})", tree.pos)
            unitLiteral
          }
          else if (arity == 1) ts.head
          else if (ctx.mode is Mode.Type) AppliedTypeTree(ref(tupleClass.typeRef), ts)
          else if (arity == 0) unitLiteral
          else Apply(ref(tupleClass.companionModule.valRef), ts)
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
    }
  }.withPos(tree.pos)

  /** Create a class definition with the same info as this refined type.
   *      parent { refinements }
   *  ==>
   *      class <refinement> extends parent { refinements }
   *
   *  The result is used for validity checking, is thrown away afterwards.
   */
  def refinedTypeToClass(tree: RefinedTypeTree)(implicit ctx: Context): TypeDef = {
    val impl = Template(emptyConstructor, tree.tpt :: Nil, EmptyValDef, tree.refinements)
    TypeDef(Modifiers(), tpnme.REFINE_CLASS, impl)
  }

 /** If tree is a variable pattern, return its name and type, otherwise return None.
   */
  private object VarPattern {
    def unapply(tree: Tree)(implicit ctx: Context): Option[VarInfo] = tree match {
      case id: Ident => Some((id, TypeTree())) // Dotty deviation: No auto-tupling
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
      case Typed(id: Ident, t) if isVarPattern(id) && id.name != nme.WILDCARD =>
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
      case SeqLiteral(elems) =>
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
