package dotty.tools
package dotc
package core

import util.Positions._, Types._, Contexts._, Constants._, Names._, NameOps._, Flags._
import SymDenotations._, Symbols._, StdNames._, Annotations._, Trees._, TypedTrees._
import TreeInfo._
import Decorators._
import language.higherKinds
import collection.mutable.ListBuffer

object UntypedTrees {

  object untpd extends Trees.Instance[Untyped] {

    // ----- Tree cases that exist in untyped form only ------------------

    /** A typed subtree of an untyped tree needs to be wrapped in a TypedSlice */
    case class TypedSplice(tree: TypedTree) extends UntypedTree

    /** mods object name impl */
    case class ModuleDef(mods: Modifiers, name: TermName, impl: Template)
      extends NameTree with ModDefTree {
      type ThisTree[T >: Untyped] <: Trees.NameTree[T] with Trees.ModDefTree[T] with ModuleDef
    }

    /** (vparams) => body */
    case class SymbolLit(str: String) extends Tree
    case class InterpolatedString(id: TermName, strings: List[Literal], elems: List[Tree]) extends Tree
    case class Function(args: List[Tree], body: Tree) extends Tree
    case class InfixOp(left: Tree, op: Name, right: Tree) extends Tree
    case class PostfixOp(tree: Tree, op: Name) extends Tree
    case class PrefixOp(op: Name, tree: Tree) extends Tree
    case class Parens(tree: Tree) extends Tree {
      def derivedParens(tree: Tree) = if (this.tree eq tree) this else Parens(tree).copyAttr(this)
    }
    case class Tuple(trees: List[Tree]) extends Tree {
      def derivedTuple(trees: List[Tree]) = if (this.trees eq trees) this else Tuple(trees).copyAttr(this)
    }
    case class WhileDo(cond: Tree, body: Tree) extends TermTree
    case class DoWhile(body: Tree, cond: Tree) extends TermTree
    case class ForYield(enums: List[Tree], expr: Tree) extends TermTree
    case class ForDo(enums: List[Tree], body: Tree) extends TermTree
    case class GenFrom(pat: Tree, expr: Tree) extends Tree
    case class GenAlias(pat: Tree, expr: Tree) extends Tree
    case class ContextBounds(bounds: TypeBoundsTree, cxBounds: List[Tree]) extends TypTree
    case class PatDef(mods: Modifiers, pats: List[Tree], tpt: Tree, rhs: Tree) extends Tree

    val unitLiteral = Literal(Constant())
  }

  import untpd._

  object Mode extends Enumeration {
    val Type, Expr, Pattern = Value
  }

  private type VarInfo = (NameTree, Tree)

  class UGen(implicit val ctx: Context) extends AnyVal {

    def scalaDot(name: Name): Select =
      Select(new TypedSplice(tpd.Ident(defn.ScalaPackageVal.termRef)), name)

    def scalaAnyRefConstr        = scalaDot(tpnme.AnyRef)
    def scalaAnyValConstr        = scalaDot(tpnme.AnyVal)
    def scalaAnyConstr           = scalaDot(tpnme.Any)
    def scalaUnitConstr          = scalaDot(tpnme.Unit)
    def productConstr            = scalaDot(tpnme.Product)
    def productConstrN(n: Int)   = scalaDot(("Product" + n).toTypeName)
    def serializableConstr       = scalaDot(tpnme.Serializable)

    def constructor(mods: Modifiers, vparamss: List[List[ValDef]], rhs: Tree = EmptyTree()): DefDef =
      DefDef(mods, nme.CONSTRUCTOR, Nil, vparamss, TypeTree(), rhs)

    def selfDef(name: TermName, tpt: Tree) =
      ValDef(Modifiers(Private), name, tpt, EmptyTree())

    def makeTupleOrParens(ts: List[Tree]) = ts match {
      case t :: Nil => Parens(t)
      case _ => Tuple(ts)
    }

    def makeTuple(ts: List[Tree]) = ts match {
      case t :: Nil => t
      case _ => Tuple(ts)
    }

    def ref(tp: NamedType)(implicit ctx: Context): Tree =
      TypedSplice(tpd.ref(tp))

    /** new C(args) */
    def New(tpt: Tree, args: List[Tree]): Apply =
      Apply(Select(Trees.New(tpt), nme.CONSTRUCTOR), args)

    def syntheticParameter(pname: TermName): ValDef =
      ValDef(Modifiers(SyntheticTermParam), pname, TypeTree(), EmptyTree())

    private def labelDefAndCall(lname: TermName, rhs: Tree, call: Tree) = {
      val ldef = DefDef(Modifiers(Label), lname, Nil, ListOfNil, TypeTree(), rhs)
      Block(ldef, call)
    }

    private def derivedValDef(mods: Modifiers, named: NameTree, tpt: Tree, rhs: Tree) =
      ValDef(mods, named.name.asTermName, tpt , rhs).withPos(named.pos)

    /** Translate infix operation expression  left op right
     */
    private def makeBinop(left: Tree, op: Name, right: Tree): Tree = {
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
        Apply(Select(left, op.encode), args)
      } else {
        val x = ctx.freshName().toTermName
        Block(
          ValDef(Modifiers(Synthetic), x, TypeTree(), left),
          Apply(Select(right, op.encode), Ident(x)))
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
    private def makeFor(mapName: TermName, flatMapName: TermName, enums: List[Tree], body: Tree): Tree = {

      /** Make a function value pat => body.
       *  If pat is a var pattern id: T then this gives (id: T) => body
       *  Otherwise this gives { case pat => body }
       */
      def makeLambda(pat: Tree, body: Tree): Tree = pat match {
        case VarPattern(named, tpt) =>
          Function(derivedValDef(Modifiers(Param), named, tpt, EmptyTree()) :: Nil, body)
        case _ =>
          Match(EmptyTree(), CaseDef(pat, EmptyTree(), body) :: Nil)
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
          CaseDef(pat, EmptyTree(), Literal(Constant(true))),
          CaseDef(Ident(nme.WILDCARD), EmptyTree(), Literal(Constant(false)))
        )
        Apply(Select(rhs, nme.withFilterIfRefutable), Match(EmptyTree(), cases))
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
          EmptyTree() //may happen for erroneous input
      }
    }

    private def makeAnnotated(cls: Symbol, tree: Tree) =
      Annotated(TypedSplice(tpd.New(cls.typeConstructor)), tree)

    /** Returns list of all pattern variables, possibly with their types,
     *  without duplicates
     */
    private def getVariables(tree: Tree): List[VarInfo] =
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
    private def makePatDef(mods: Modifiers, pat: Tree, rhs: Tree): Tree = pat match {
      case VarPattern(named, tpt) =>
        derivedValDef(mods, named, tpt, rhs)
      case _ =>
        val rhsUnchecked = makeAnnotated(defn.UncheckedAnnot, rhs)
        val vars = getVariables(pat)
        val ids = for ((named, _) <- vars) yield Ident(named.name)
        val caseDef = CaseDef(pat, EmptyTree(), makeTuple(ids))
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
            TempTrees(firstDef :: restDefs)
        }
    }

    def desugarContextBounds(tparams: List[TypeDef], vparamss: List[List[ValDef]], ofClass: Boolean): (List[TypeDef], List[List[ValDef]]) = {
      val epbuf = new ListBuffer[ValDef]
      def makeEvidenceParam(cxBound: Tree): ValDef = ???
      val tparams1 = tparams map {
        case tparam @ TypeDef(mods, name, ttparams, ContextBounds(tbounds, cxbounds)) =>
          for (cxbound <- cxbounds) {
            val accessMods = if (ofClass) PrivateOrLocal else EmptyFlags
            val epname = (nme.EVIDENCE_PARAM_PREFIX.toString + epbuf.length).toTermName
            epbuf +=
              ValDef(Modifiers(Implicit | Param | accessMods), epname, cxbound, EmptyTree())
          }
          tparam.derivedTypeDef(mods, name, ttparams, tbounds)
        case tparam =>
          tparam
      }
      val evidenceParams = epbuf.toList
      val vparamss1 = vparamss.reverse match {
        case (vparams @ (vparam :: _)) :: rvparamss if vparam.mods is Implicit =>
          ((vparams ++ evidenceParams) :: rvparamss).reverse
        case _ =>
          vparamss :+ evidenceParams
      }
      (tparams1, vparamss1)
    }

    def desugarContextBounds(tree: Tree): Tree = tree match {
      case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
        val (tparams1, vparamss1) =
          desugarContextBounds(tparams, vparamss, ofClass = false)
        tree.derivedDefDef(mods, name, tparams1, vparamss, tpt, rhs)
      case ClassDef(
        mods, name, tparams, templ @ Template(constr, parents, self, body)) =>
        val (tparams1, vparamss1) =
          desugarContextBounds(tparams, constr.vparamss, ofClass = true)
        val constr1 = constr.derivedDefDef(
          constr.mods, constr.name, constr.tparams, vparamss1, constr.tpt, constr.rhs)
        val templ1 = templ.derivedTemplate(constr1, parents, self, body)
        tree.derivedClassDef(mods, name, tparams1, templ1)
      case _ => tree
    }

    def desugarAnonClass(templ: Template): Tree = {
      val x = tpnme.ANON_CLASS
      val clsDef = ClassDef(Modifiers(Final), x, Nil, templ)
      Block(clsDef, New(Ident(x), Nil))
    }

    def desugar(tree: Tree, mode: Mode.Value): Tree = {
      def isPatternVar(id: Ident) =
        mode == Mode.Pattern && isVarPattern(id) && id.name != nme.WILDCARD
      tree match { // todo: move general tree desugaring to typer, and keep only untyped trees here?
        case id @ Ident(_) if isPatternVar(id) =>
          Bind(id.name, Ident(nme.WILDCARD))
        case Typed(id @ Ident(_), tpt) if isPatternVar(id) =>
          Bind(id.name, Typed(Ident(nme.WILDCARD), tpt)).withPos(id.pos)
        case New(templ: Template) =>
          desugarAnonClass(templ)
        case Assign(Apply(fn, args), rhs) =>
          Apply(Select(fn, nme.update), args :+ rhs)
        case If(cond, thenp, EmptyTree()) =>
          If(cond, thenp, unitLiteral)
        case _: DefDef| _: ClassDef =>
          desugarContextBounds(tree)
        case ModuleDef(mods, name, tmpl @ Template(constr, parents, self, body)) =>
          // <module> val name: name$ = New(name$)
          // <module> final class name$ extends parents { self: name.type => body }
          val clsName = name.moduleClassName
          val clsRef = Ident(clsName)
          val modul = ValDef(mods | ModuleCreationFlags, name, clsRef, clsRef)
          val clsSelf = self.derivedValDef(self.mods, self.name, SingletonTypeTree(Ident(name)), self.rhs)
          val clsTmpl = tmpl.derivedTemplate(constr, parents, clsSelf, body)
          val cls = ClassDef(mods & AccessFlags | ModuleClassCreationFlags, clsName, Nil, clsTmpl)
          TempTrees(List(modul, cls))
        case SymbolLit(str) =>
          New(ref(defn.SymbolClass.typeConstructor), Literal(Constant(str)) :: Nil)
        case InterpolatedString(id, strs, elems) =>
          Apply(Select(Apply(Ident(nme.StringContext), strs), id), elems)
        case Function(args, body) =>
          if (mode == Mode.Type) // FunctionN[args: _*, body]
            AppliedTypeTree(
              ref(defn.FunctionClass(args.length).typeConstructor),
              args :+ body)
          else { // { def $anonfun(args) = body; $anonfun }
            val params = args.asInstanceOf[List[ValDef]]
            Block(
              DefDef(Modifiers(Synthetic), nme.ANON_FUN, Nil, params :: Nil, EmptyTree(), body),
              Ident(nme.ANON_FUN)
            )
          }
        case InfixOp(l, op, r) =>
          mode match {
            case Mode.Expr => // l.op'(r), or val x = r; l.op;(x), plus handle named args specially
              makeBinop(l, op, r)
            case Mode.Pattern => // op'(l, r)
              Apply(Ident(op.encode), l :: r :: Nil)
            case Mode.Type => // op'[l, r]
              AppliedTypeTree(Ident(op.encode), l :: r :: Nil)
          }
        case PostfixOp(t, op) =>
          if (mode == Mode.Type && op == nme.raw.STAR)
            AppliedTypeTree(ref(defn.RepeatedParamType), t)
          else {
            assert(mode == Mode.Expr)
            if (op == nme.WILDCARD) tree // desugar later by eta expansion
            else Select(t, op.encode)
          }
        case PrefixOp(op, t) =>
          if (mode == Mode.Type && op == nme.ARROWkw)
            AppliedTypeTree(ref(defn.ByNameParamClass.typeConstructor), t)
          else
            Select(t, nme.UNARY_PREFIX ++ op.encode)
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
          combine(pats1 map (makePatDef(mods, _, rhs)))
        case _ =>
          tree
      }
    }.withPos(tree.pos)
  }

  def ugen(implicit ctx: Context) = new UGen

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
}