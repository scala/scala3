package dotty.tools
package dotc
package ast

import core._
import util.Positions._, Types._, Contexts._, Constants._, Names._, NameOps._, Flags._
import SymDenotations._, Symbols._, StdNames._, Annotations._, Trees._
import TreeInfo._
import Decorators._
import language.higherKinds
import collection.mutable.ListBuffer

object untpd extends Trees.Instance[Untyped] {

  val EmptyTree = emptyTree[Untyped]()

// ----- Tree cases that exist in untyped form only ------------------

  /** A typed subtree of an untyped tree needs to be wrapped in a TypedSlice */
  case class TypedSplice(tree: tpd.Tree) extends Tree

  /** mods object name impl */
  case class ModuleDef(mods: Modifiers, name: TermName, impl: Template)
    extends NameTree with MemberDef {
    type ThisTree[T >: Untyped] <: Trees.NameTree[T] with Trees.MemberDef[T] with ModuleDef
    def withName(name: Name) = this.derivedModuleDef(mods, name.toTermName, impl)
  }

  case class SymbolLit(str: String) extends Tree
  case class InterpolatedString(id: TermName, strings: List[Literal], elems: List[Tree]) extends Tree
  case class Function(args: List[Tree], body: Tree) extends Tree
  case class InfixOp(left: Tree, op: Name, right: Tree) extends Tree
  case class PostfixOp(od: Tree, op: Name) extends Tree
  case class PrefixOp(op: Name, od: Tree) extends Tree
  case class Parens(t: Tree) extends Tree
  case class Tuple(trees: List[Tree]) extends Tree
  case class WhileDo(cond: Tree, body: Tree) extends TermTree
  case class DoWhile(body: Tree, cond: Tree) extends TermTree
  case class ForYield(enums: List[Tree], expr: Tree) extends TermTree
  case class ForDo(enums: List[Tree], body: Tree) extends TermTree
  case class GenFrom(pat: Tree, expr: Tree) extends Tree
  case class GenAlias(pat: Tree, expr: Tree) extends Tree
  case class ContextBounds(bounds: TypeBoundsTree, cxBounds: List[Tree]) extends TypTree
  case class PatDef(mods: Modifiers, pats: List[Tree], tpt: Tree, rhs: Tree) extends Tree

// ------ Untyped tree values and creation methods ---------------------

  private type VarInfo = (NameTree, Tree)

  val unitLiteral = Literal(Constant())

  def ref(tp: NamedType)(implicit ctx: Context): Tree =
    TypedSplice(tpd.ref(tp))

  def scalaUnit(implicit ctx: Context) = ref(defn.UnitClass.typeConstructor)

  def makeConstructor(mods: Modifiers, vparamss: List[List[ValDef]], rhs: Tree = EmptyTree)(implicit ctx: Context): DefDef =
    DefDef(mods, nme.CONSTRUCTOR, Nil, vparamss, TypeTree(), rhs)

  def emptyConstructor(implicit ctx: Context): DefDef =
    makeConstructor(Modifiers(), Nil)

  def makeSelfDef(name: TermName, tpt: Tree)(implicit ctx: Context) =
    ValDef(Modifiers(Private), name, tpt, EmptyTree)

  def makeTupleOrParens(ts: List[Tree])(implicit ctx: Context) = ts match {
    case t :: Nil => Parens(t)
    case _ => Tuple(ts)
  }

  def makeTuple(ts: List[Tree])(implicit ctx: Context) = ts match {
    case t :: Nil => t
    case _ => Tuple(ts)
  }

  def makeParameter(pname: TermName, tpe: Tree, mods: Modifiers = Modifiers()): ValDef =
    ValDef(mods | Param, pname, tpe, emptyTree())

  def makeSyntheticParameter(n: Int = 1, tpt: Tree = EmptyTree)(implicit ctx: Context): ValDef =
    ValDef(Modifiers(SyntheticTermParam), nme.syntheticParamName(n), TypeTree(), EmptyTree)

  def refOfDef(tree: NameTree) = Ident(tree.name)

// ------ Untyped tree desugaring ------------------------------------------

  def desugar(tree: Tree, mode: Mode.Value)(implicit ctx: Context): Tree = {

    def labelDefAndCall(lname: TermName, rhs: Tree, call: Tree) = {
      val ldef = DefDef(Modifiers(Label), lname, Nil, ListOfNil, TypeTree(), rhs)
      Block(ldef, call)
    }

    def derivedValDef(mods: Modifiers, named: NameTree, tpt: Tree, rhs: Tree) =
      ValDef(mods, named.name.asTermName, tpt, rhs).withPos(named.pos)

    /** Translate infix operation expression  left op right
     */
    def makeBinop(left: Tree, op: Name, right: Tree): Tree = {
      def assignToNamedArg(arg: Tree) = arg match {
        case Assign(Ident(name), rhs) => arg.derivedNamedArg(name, rhs)
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

    /** Make closure corresponding to function  params => body */
    def makeClosure(params: List[ValDef], body: Tree) =
      Block(
        DefDef(Modifiers(Synthetic), nme.ANON_FUN, Nil, params :: Nil, EmptyTree, body),
        Closure(Nil, Ident(nme.ANON_FUN)))

    /** Make closure corresponding to partial function  { cases } */
    def makeCaseClosure(cases: List[CaseDef]) = {
      val param = makeSyntheticParameter()
      makeClosure(param :: Nil, Match(Ident(param.name), cases))
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
    def makeFor(mapName: TermName, flatMapName: TermName, enums: List[Tree], body: Tree): Tree = {

      /** Make a function value pat => body.
       *  If pat is a var pattern id: T then this gives (id: T) => body
       *  Otherwise this gives { case pat => body }
       */
      def makeLambda(pat: Tree, body: Tree): Tree = pat match {
        case VarPattern(named, tpt) =>
          makeClosure(derivedValDef(Modifiers(Param), named, tpt, EmptyTree) :: Nil, body)
        case _ =>
          makeCaseClosure(CaseDef(pat, EmptyTree, body) :: Nil)
      }

      /** If `pat` is not yet a `Bind` wrap it in one with a fresh name
       */
      def makeBind(pat: Tree): Tree = pat match {
        case Bind(_, _) => pat
        case _ => Bind(ctx.freshName().toTermName, pat)
      }

      /** Is pattern `pat` irrefutable when matched against `rhs`?
       *  We only can do a simple syntactic check here; a more refined check
       *  is done later prompted by the presence of a "withFilterIfRefutable" call.
       */
      def isIrrefutable(pat: Tree, rhs: Tree): Boolean = {
        def matchesTuple(pats: List[Tree], rhs: Tree): Boolean = rhs match {
          case Tuple(trees) => (pats corresponds trees)(isIrrefutable)
          case Parens(rhs1) => matchesTuple(pats, rhs1)
          case Block(_, rhs1) => matchesTuple(pats, rhs1)
          case If(_, thenp, elsep) => matchesTuple(pats, thenp) && matchesTuple(pats, elsep)
          case Match(_, cases) => cases forall (matchesTuple(pats, _))
          case CaseDef(_, _, rhs1) => matchesTuple(pats, rhs)
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

      /** Make a pattern filter:
       *    rhs.withFilterIfRefutable { case pat => true case _ => false }
       */
      def makePatFilter(rhs: Tree, pat: Tree): Tree = {
        val cases = List(
          CaseDef(pat, EmptyTree, Literal(Constant(true))),
          CaseDef(Ident(nme.WILDCARD), EmptyTree, Literal(Constant(false))))
        Apply(Select(rhs, nme.withFilterIfRefutable), Match(EmptyTree, cases))
      }

      /** rhs.name with a pattern filter on rhs unless `pat` is irrefutable when
       *  matched against `rhs`.
       */
      def rhsSelect(rhs: Tree, name: TermName, pat: Tree) = {
        val rhs1 = if (isIrrefutable(pat, rhs)) rhs else makePatFilter(rhs, pat)
        Select(rhs1, name)
      }

      enums match {
        case (enum @ GenFrom(pat, rhs)) :: Nil =>
          Apply(rhsSelect(rhs, mapName, pat), makeLambda(pat, body))
        case GenFrom(pat, rhs) :: (rest @ (GenFrom(_, _) :: _)) =>
          val cont = makeFor(mapName, flatMapName, rest, body)
          Apply(rhsSelect(rhs, flatMapName, pat), makeLambda(pat, cont))
        case (enum @ GenFrom(pat, rhs)) :: (rest @ GenAlias(_, _) :: _) =>
          val (valeqs, rest1) = rest.span(_.isInstanceOf[GenAlias])
          val pats = valeqs map { case GenAlias(pat, _) => pat }
          val rhss = valeqs map { case GenAlias(_, rhs) => rhs }
          val defpat1 = makeBind(pat)
          val defpats = pats map makeBind
          val pdefs = (defpats, rhss).zipped map (makePatDef(Modifiers(), _, _))
          val ids = (defpat1 :: defpats) map { case Bind(name, _) => Ident(name) }
          val rhs1 = makeFor(nme.map, nme.flatMap, GenFrom(defpat1, rhs) :: Nil, Block(pdefs, makeTuple(ids)))
          val allpats = pat :: pats
          val vfrom1 = GenFrom(makeTuple(allpats), rhs1)
          makeFor(mapName, flatMapName, vfrom1 :: rest1, body)
        case (enum @ GenFrom(pat, rhs)) :: test :: rest =>
          val filtered = Apply(rhsSelect(rhs, nme.withFilter, pat), makeLambda(pat, test))
          makeFor(mapName, flatMapName, GenFrom(pat, filtered) :: rest, body)
        case _ =>
          EmptyTree //may happen for erroneous input
      }
    }

    def makeAnnotated(cls: Symbol, tree: Tree) =
      Annotated(TypedSplice(tpd.New(cls.typeConstructor)), tree)

    /** Returns list of all pattern variables, possibly with their types,
     *  without duplicates
     */
    def getVariables(tree: Tree): List[VarInfo] =
      getVars(new ListBuffer[VarInfo], tree).toList

    /** In case there is exactly one variable x_1 in pattern
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
    def makePatDef(mods: Modifiers, pat: Tree, rhs: Tree): Tree = pat match {
      case VarPattern(named, tpt) =>
        derivedValDef(mods, named, tpt, rhs)
      case _ =>
        val rhsUnchecked = makeAnnotated(defn.UncheckedAnnot, rhs)
        val vars = getVariables(pat)
        val ids = for ((named, _) <- vars) yield Ident(named.name)
        val caseDef = CaseDef(pat, EmptyTree, makeTuple(ids))
        val matchExpr = Match(rhsUnchecked, caseDef :: Nil)
        vars match {
          case (named, tpt) :: Nil =>
            derivedValDef(mods, named, tpt, matchExpr)
          case _ =>
            val tmpName = ctx.freshName().toTermName
            val patMods = Modifiers(PrivateLocal | Synthetic | (mods.flags & Lazy))
            val firstDef = ValDef(patMods, tmpName, TypeTree(), matchExpr)
            def selector(n: Int) = Select(Ident(tmpName), ("_" + n).toTermName)
            val restDefs =
              for (((named, tpt), n) <- vars.zipWithIndex)
                yield derivedValDef(mods, named, tpt, selector(n))
            Thicket(firstDef :: restDefs)
        }
    }

    def isPatternVar(id: Ident) =
      mode == Mode.Pattern && isVarPattern(id) && id.name != nme.WILDCARD

    // begin desugar
    val tree1 = tree match { // todo: move general tree desugaring to typer, and keep only untyped trees here?
      case id @ Ident(_) if isPatternVar(id) =>
        Bind(id.name, Ident(nme.WILDCARD))
      case Typed(id @ Ident(_), tpt) if isPatternVar(id) =>
        Bind(id.name, Typed(Ident(nme.WILDCARD), tpt)).withPos(id.pos)
      case New(templ: Template) =>
        desugarAnonClass(templ)
      case Assign(Apply(fn, args), rhs) =>
        Apply(Select(fn, nme.update), args :+ rhs)
      case If(cond, thenp, EmptyTree) =>
        If(cond, thenp, unitLiteral)
      case Match(EmptyTree, cases) =>
        makeCaseClosure(cases)
      case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
        val (tparams1, vparamss1) =
          desugarContextBounds(tparams, vparamss, ofClass = false)
        tree.derivedDefDef(mods, name, tparams1, vparamss1, tpt, rhs)
      case ClassDef(
        mods, name, tparams, impl @ Template(constr, parents, self, body)) =>
        val (tparams1, vparamss1) =
          desugarContextBounds(tparams, constr.vparamss, ofClass = true)
        val constr1 = constr.derivedDefDef(
          constr.mods, constr.name, constr.tparams, vparamss1, constr.tpt, constr.rhs)

        val templ1 = impl.derivedTemplate(constr1, parents, self, body)
        tree.derivedClassDef(mods, name, tparams1, templ1)
      case ModuleDef(mods, name, tmpl @ Template(constr, parents, self, body)) =>
        // <module> val name: name$ = New(name$)
        // <module> final class name$ extends parents { self: name.type => body }
        val clsName = name.moduleClassName
        val clsRef = Ident(clsName)
        val modul = ValDef(mods | ModuleCreationFlags, name, clsRef, New(clsRef, Nil))
        val clsSelf = self.derivedValDef(self.mods, self.name, SingletonTypeTree(Ident(name)), self.rhs)
        val clsTmpl = tmpl.derivedTemplate(constr, parents, clsSelf, body)
        val cls = ClassDef(mods.toTypeFlags & AccessFlags | ModuleClassCreationFlags, clsName, Nil, clsTmpl)
        Thicket(modul, cls)
      case SymbolLit(str) =>
        New(ref(defn.SymbolClass.typeConstructor), (Literal(Constant(str)) :: Nil) :: Nil)
      case InterpolatedString(id, strs, elems) =>
        Apply(Select(Apply(Ident(nme.StringContext), strs), id), elems)
      case Function(args, body) =>
        if (mode == Mode.Type) // FunctionN[args: _*, body]
          AppliedTypeTree(
            ref(defn.FunctionClass(args.length).typeConstructor),
            args :+ body)
        else
          makeClosure(args.asInstanceOf[List[ValDef]], body)
      case InfixOp(l, op, r) =>
        mode match {
          case Mode.Expr => // l.op(r), or val x = r; l.op(x), plus handle named args specially
            makeBinop(l, op, r)
          case Mode.Pattern => // op(l, r)
            Apply(Ident(op), l :: r :: Nil)
          case Mode.Type => // op[l, r]
            AppliedTypeTree(Ident(op), l :: r :: Nil)
        }
      case PostfixOp(t, op) =>
        if (mode == Mode.Type && op == nme.raw.STAR)
          AppliedTypeTree(ref(defn.RepeatedParamType), t)
        else {
          assert(mode == Mode.Expr)
          if (op == nme.WILDCARD) tree // desugar later by eta expansion
          else Select(t, op)
        }
      case PrefixOp(op, t) =>
        if (mode == Mode.Type && op == nme.ARROWkw)
          AppliedTypeTree(ref(defn.ByNameParamClass.typeConstructor), t)
        else
          Select(t, nme.UNARY_PREFIX ++ op)
      case Parens(t) =>
        t
      case Tuple(ts) =>
        def PairTypeTree(l: Tree, r: Tree) =
          AppliedTypeTree(ref(defn.PairClass.typeConstructor), l :: r :: Nil)
        if (mode == Mode.Type) ts.reduceRight(PairTypeTree)
        else if (ts.isEmpty) unitLiteral
        else ts.reduceRight(Pair(_, _))
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
        Thicket(pats1 map (makePatDef(mods, _, rhs)))
      case _ =>
        tree
    }
    tree1 match {
      case tree1: NameTree => tree1.withName(tree1.name.encode)
      case _ => tree1
    }
  }.withPos(tree.pos)

  def desugarContextBounds(tparams: List[TypeDef], vparamss: List[List[ValDef]], ofClass: Boolean): (List[TypeDef], List[List[ValDef]]) = {
    val epbuf = new ListBuffer[ValDef]
    def makeEvidenceParam(cxBound: Tree): ValDef = ???
    val tparams1 = tparams mapConserve {
      case tparam @ TypeDef(mods, name, ttparams, ContextBounds(tbounds, cxbounds)) =>
        for (cxbound <- cxbounds) {
          val accessMods = if (ofClass) PrivateOrLocal else EmptyFlags
          val epname = (nme.EVIDENCE_PARAM_PREFIX.toString + epbuf.length).toTermName
          epbuf +=
            ValDef(Modifiers(Implicit | Param | accessMods), epname, cxbound, EmptyTree)
        }
        tparam.derivedTypeDef(mods, name, ttparams, tbounds)
      case tparam =>
        tparam
    }
    epbuf.toList match {
      case Nil =>
        (tparams, vparamss)
      case evidenceParams =>
        val vparamss1 = vparamss.reverse match {
          case (vparams @ (vparam :: _)) :: rvparamss if vparam.mods is Implicit =>
            ((vparams ++ evidenceParams) :: rvparamss).reverse
          case _ =>
            vparamss :+ evidenceParams
        }
        (tparams1, vparamss1)
    }
  }

  val NotInTypeAccessorFlags = Param | Private | Local

  def desugarClassDef(cdef: ClassDef)(implicit ctx: Context): ClassDef = {
    val ClassDef(
      mods, name, tparams, impl @ Template(constr, parents, self, body)) = cdef

    // desugar context bounds
    val (tparamAccs, vparamAccss) =
      desugarContextBounds(tparams, constr.vparamss, ofClass = true)

    val tparams1 = tparams.map(tparam => tparam.derivedTypeDef(
      Modifiers(Param), tparam.name, tparam.tparams, tparam.rhs))

    // ensure parameter list is non-empty
    val vparamss1 =
      if (vparamAccss.isEmpty) {
        if (mods is Case)
          ctx.error("case class needs to have at least one parameter list", cdef.pos)
        ListOfNil
      }
      else
        vparamAccss.nestedMap(vparam => vparam.derivedValDef(
          Modifiers(Param), vparam.name, vparam.tpt, vparam.rhs))

    val constr1 = constr.derivedDefDef(
      constr.mods, constr.name, constr.tparams, vparamss1, constr.tpt, constr.rhs)

    cdef.derivedClassDef(mods, name, tparams1,
      impl.derivedTemplate(constr1, parents, self, tparamAccs ::: vparamAccss.flatten ::: body))
  }

  /** Expand to:
   *  <module> val name: name$ = New(name$)
   *  <module> final class name$ extends parents { self: name.type => body }
   */
  def desugarModuleDef(mdef: ModuleDef): Tree = {
    val ModuleDef(mods, name, tmpl @ Template(constr, parents, self, body)) = mdef
    val clsName = name.moduleClassName
    val clsRef = Ident(clsName)
    val modul = ValDef(mods | ModuleCreationFlags, name, clsRef, New(clsRef, Nil))
    val clsSelf = self.derivedValDef(self.mods, self.name, SingletonTypeTree(Ident(name)), self.rhs)
    val clsTmpl = tmpl.derivedTemplate(constr, parents, clsSelf, body)
    val cls = ClassDef(mods.toTypeFlags & AccessFlags | ModuleClassCreationFlags, clsName, Nil, clsTmpl)
    Thicket(modul, cls)
  }

  def desugarAnonClass(templ: Template): Tree = {
    val x = tpnme.ANON_CLASS
    val clsDef = ClassDef(Modifiers(Final), x, Nil, templ)
    Block(clsDef, New(Ident(x), Nil))
  }

  object Mode extends Enumeration {
    val Type, Expr, Pattern = Value
  }

  /** If tree is a variable pattern, return its name and type, otherwise return None.
   */
  private object VarPattern {
    def unapply(tree: Tree): Option[VarInfo] = tree match {
      case id: Ident => Some(id, TypeTree())
      case Typed(id: Ident, tpt) => Some((id, tpt))
      case _ => None
    }
  }

  /** Traverse pattern and collect all variable names with their types in buffer.
   *  Works for expanded as well as unexpanded patterns
   *
   */
  private object getVars extends TreeAccumulator[ListBuffer[VarInfo]] {
    override def apply(buf: ListBuffer[VarInfo], tree: Tree): ListBuffer[VarInfo] = {
      def seenName(name: Name) = buf exists (_._1.name == name)
      def add(named: NameTree, t: Tree): ListBuffer[VarInfo] =
        if (seenName(named.name)) buf else buf += ((named, t))
      tree match {
        case Bind(nme.WILDCARD, _) =>
          foldOver(buf, tree)
        case tree @ Bind(_, Typed(tree1, tpt)) if !mayBeTypePat(tpt) =>
          apply(add(tree, tpt), tree1)
        case tree @ Bind(_, tree1) =>
          apply(add(tree, TypeTree()), tree1)
        case Typed(id: Ident, t) if isVarPattern(id) =>
          add(id, t)
        case id: Ident if isVarPattern(id) =>
          add(id, TypeTree())
        case _ =>
          foldOver(buf, tree)
      }
    }
  }

// ------- A decorator for producing a path to a location --------------

  implicit class UntypedTreeDecorator(val self: Tree) extends AnyVal {
    def locateEnclosing(base: List[Tree], pos: Position): List[Tree] = {
      def encloses(elem: Any) = elem match {
        case t: Tree => t.envelope contains pos
        case _ => false
      }
      base.productIterator find encloses match {
        case Some(tree: Tree) => locateEnclosing(tree :: base, pos)
        case none => base
      }
    }
  }

// --------- Copier/Transformer/Accumulator classes for untyped trees -----

  implicit class UntypedTreeCopier(val tree: Tree) extends AnyVal {
    def derivedModuleDef(mods: Modifiers, name: TermName, impl: Template) = tree match {
      case tree: ModuleDef if (mods eq tree.mods) && (name eq tree.name) && (impl eq tree.impl) =>tree
      case _ => ModuleDef(mods, name, impl).copyAttr(tree)
    }
    def derivedSymbolLit(str: String) = tree match {
      case tree: SymbolLit if (str == tree.str) => tree
      case _ => SymbolLit(str).copyAttr(tree)
    }
    def derivedInterpolatedString(id: TermName, strings: List[Literal], elems: List[Tree]) = tree match {
      case tree: InterpolatedString if (id eq tree.id) && (strings eq tree.strings) && (elems eq tree.elems) => tree
      case _ => InterpolatedString(id, strings, elems).copyAttr(tree)
    }
    def derivedFunction(args: List[Tree], body: Tree) = tree match {
      case tree: Function if (args eq tree.args) && (body eq tree.body) => tree
      case _ => Function(args, body).copyAttr(tree)
    }
    def derivedInfixOp(left: Tree, op: Name, right: Tree) = tree match {
      case tree: InfixOp if (left eq tree.left) && (op eq tree.op) && (right eq tree.right) => tree
      case _ => InfixOp(left, op, right).copyAttr(tree)
    }
    def derivedPostfixOp(od: Tree, op: Name) = tree match {
      case tree: PostfixOp if (od eq tree.od) && (op eq tree.op) => tree
      case _ => PostfixOp(od, op).copyAttr(tree)
    }
    def derivedPrefixOp(op: Name, od: Tree) = tree match {
      case tree: PrefixOp if (op eq tree.op) && (od eq tree.od) => tree
      case _ => PrefixOp(op, od).copyAttr(tree)
    }
    def derivedParens(t: Tree) = tree match {
      case tree: Parens if (t eq tree.t) => tree
      case _ => Parens(t).copyAttr(tree)
    }
    def derivedTuple(trees: List[Tree]) = tree match {
      case tree: Tuple if (trees eq tree.trees) => tree
      case _ => Tuple(trees).copyAttr(tree)
    }
    def derivedWhileDo(cond: Tree, body: Tree) = tree match {
      case tree: WhileDo if (cond eq tree.cond) && (body eq tree.body) => tree
      case _ => WhileDo(cond, body).copyAttr(tree)
    }
    def derivedDoWhile(body: Tree, cond: Tree) = tree match {
      case tree: DoWhile if (body eq tree.body) && (cond eq tree.cond) => tree
      case _ => DoWhile(body, cond).copyAttr(tree)
    }
    def derivedForYield(enums: List[Tree], expr: Tree) = tree match {
      case tree: ForYield if (enums eq tree.enums) && (expr eq tree.expr) => tree
      case _ => ForYield(enums, expr).copyAttr(tree)
    }
    def derivedForDo(enums: List[Tree], body: Tree) = tree match {
      case tree: ForDo if (enums eq tree.enums) && (body eq tree.body) => tree
      case _ => ForDo(enums, body).copyAttr(tree)
    }
    def derivedGenFrom(pat: Tree, expr: Tree) = tree match {
      case tree: GenFrom if (pat eq tree.pat) && (expr eq tree.expr) => tree
      case _ => GenFrom(pat, expr).copyAttr(tree)
    }
    def derivedGenAlias(pat: Tree, expr: Tree) = tree match {
      case tree: GenAlias if (pat eq tree.pat) && (expr eq tree.expr) => tree
      case _ => GenAlias(pat, expr).copyAttr(tree)
    }
    def derivedContextBounds(bounds: TypeBoundsTree, cxBounds: List[Tree]) = tree match {
      case tree: ContextBounds if (bounds eq tree.bounds) && (cxBounds eq tree.cxBounds) => tree
      case _ => ContextBounds(bounds, cxBounds).copyAttr(tree)
    }
    def derivedPatDef(mods: Modifiers, pats: List[Tree], tpt: Tree, rhs: Tree) = tree match {
      case tree: PatDef if (mods eq tree.mods) && (pats eq tree.pats) && (tpt eq tree.tpt) && (rhs eq tree.rhs) => tree
      case _ => PatDef(mods, pats, tpt, rhs).copyAttr(tree)
    }
  }

  abstract class TreeTransformer extends Trees.TreeTransformer[Untyped] {
    override def transform(tree: Tree): Tree = tree match {
      case ModuleDef(mods, name, impl) =>
        tree.derivedModuleDef(mods, name, transformSub(impl))
      case SymbolLit(str) =>
        tree.derivedSymbolLit(str)
      case InterpolatedString(id, strings, elems) =>
        tree.derivedInterpolatedString(id, transformSub(strings), transform(elems))
      case Function(args, body) =>
        tree.derivedFunction(transform(args), transform(body))
      case InfixOp(left, op, right) =>
        tree.derivedInfixOp(transform(left), op, transform(right))
      case PostfixOp(od, op) =>
        tree.derivedPostfixOp(transform(od), op)
      case PrefixOp(op, od) =>
        tree.derivedPrefixOp(op, transform(od))
      case Parens(t) =>
        tree.derivedParens(transform(t))
      case Tuple(trees) =>
        tree.derivedTuple(transform(trees))
      case WhileDo(cond, body) =>
        tree.derivedWhileDo(transform(cond), transform(body))
      case DoWhile(body, cond) =>
        tree.derivedDoWhile(transform(body), transform(cond))
      case ForYield(enums, expr) =>
        tree.derivedForYield(transform(enums), transform(expr))
      case ForDo(enums, body) =>
        tree.derivedForDo(transform(enums), transform(body))
      case GenFrom(pat, expr) =>
        tree.derivedGenFrom(transform(pat), transform(expr))
      case GenAlias(pat, expr) =>
        tree.derivedGenAlias(transform(pat), transform(expr))
      case ContextBounds(bounds, cxBounds) =>
        tree.derivedContextBounds(transformSub(bounds), transform(cxBounds))
      case PatDef(mods, pats, tpt, rhs) =>
        tree.derivedPatDef(mods, transform(pats), transform(tpt), transform(rhs))
      case _ =>
        super.transform(tree)
    }
  }

  abstract class TreeAccumulator[X] extends Trees.TreeAccumulator[X, Untyped] {
    override def foldOver(x: X, tree: Tree): X = tree match {
      case ModuleDef(mods, name, impl) =>
        this(x, impl)
      case SymbolLit(str) =>
        x
      case InterpolatedString(id, strings, elems) =>
        this(this(x, strings), elems)
      case Function(args, body) =>
        this(this(x, args), body)
      case InfixOp(left, op, right) =>
        this(this(x, left), right)
      case PostfixOp(od, op) =>
        this(x, od)
      case PrefixOp(op, od) =>
        this(x, od)
      case Parens(t) =>
        this(x, t)
      case Tuple(trees) =>
        this(x, trees)
      case WhileDo(cond, body) =>
        this(this(x, cond), body)
      case DoWhile(body, cond) =>
        this(this(x, body), cond)
      case ForYield(enums, expr) =>
        this(this(x, enums), expr)
      case ForDo(enums, body) =>
        this(this(x, enums), body)
      case GenFrom(pat, expr) =>
        this(this(x, pat), expr)
      case GenAlias(pat, expr) =>
        this(this(x, pat), expr)
      case ContextBounds(bounds, cxBounds) =>
        this(this(x, bounds), cxBounds)
      case PatDef(mods, pats, tpt, rhs) =>
        this(this(this(x, pats), tpt), rhs)
      case _ =>
        super.foldOver(x, tree)
    }
  }
}
