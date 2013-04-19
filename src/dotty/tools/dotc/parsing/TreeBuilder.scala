package dotty.tools
package dotc
package parsing

import core._
import Flags._, Trees._, TypedTrees._, UntypedTrees._, Names._, StdNames._, NameOps._, Contexts._
import scala.collection.mutable.ListBuffer
import util.Positions._, Symbols._, Decorators._, Flags._, Constants._

/** Methods for building trees, used in the parser.  All the trees
 *  returned by this class must be untyped.
 */
abstract class TreeBuilder(implicit ctx: Context) {

  import untpd._

  def o2p(offset: Int): Position
  def r2p(start: Int, point: Int, end: Int): Position

  def scalaDot(name: Name)(implicit cpos: Position): Select =
    Select(new TypedSplice(tpd.Ident(defn.ScalaPackageVal.termRef)), name)

  def scalaAnyRefConstr(implicit cpos: Position)        = scalaDot(tpnme.AnyRef)
  def scalaAnyValConstr(implicit cpos: Position)        = scalaDot(tpnme.AnyVal)
  def scalaAnyConstr(implicit cpos: Position)           = scalaDot(tpnme.Any)
  def scalaUnitConstr(implicit cpos: Position)          = scalaDot(tpnme.Unit)
  def productConstr(implicit cpos: Position)            = scalaDot(tpnme.Product)
  def productConstrN(n: Int)(implicit cpos: Position)   = scalaDot(("Product" + n).toTypeName)
  def serializableConstr(implicit cpos: Position)       = scalaDot(tpnme.Serializable)

  def convertToTypeName(t: Tree) = ???

  implicit val cpos = NoPosition

  /** Convert all occurrences of (lower-case) variables in a pattern as follows:
   *    x                  becomes      x @ _
   *    x: T               becomes      x @ (_: T)
   */
  private object patvarTransformer extends TreeTransformer {
    override def transform(tree: Tree): Tree = tree match {
      case Ident(name) if (treeInfo.isVarPattern(tree) && name != nme.WILDCARD) =>
        Bind(
          name, Ident(nme.WILDCARD)(tree.pos.focus)
        )(tree.pos)
      case Typed(id @ Ident(name), tpt) if (treeInfo.isVarPattern(id) && name != nme.WILDCARD) =>
        Bind(
          name,
          Typed(
            Ident(nme.WILDCARD)(tree.pos.focus),
            tpt
          )(tree.pos.withStart(tree.pos.point))
        )(tree.pos.withPoint(id.pos.point))
      case Apply(fn @ Apply(_, _), args) =>
        tree.derivedApply(transform(fn), transform(args))
      case Apply(fn, args) =>
        tree.derivedApply(fn, transform(args))
      case Typed(expr, tpt) =>
        tree.derivedTyped(transform(expr), tpt)
      case Bind(name, body) =>
        tree.derivedBind(name, transform(body))
      case Alternative(_) =>
        super.transform(tree)
      case _ =>
        tree
    }
  }

case class VariableInfo(name: Name, tree: Tree, pos: Position)

  /** Traverse pattern and collect all variable names with their types in buffer
   *  The variables keep their positions; whereas the pattern is converted to be
   *  synthetic for all nodes that contain a variable position.
   */
  object getVars extends TreeAccumulator[ListBuffer[VariableInfo]] {

    def namePos(tree: Tree, name: Name): Position =
      if (name contains '$') tree.pos.focus
      else {
        val start = tree.pos.start
        val end = start + name.decode.length
        Position(start, end)
      }

    override def apply(buf: ListBuffer[VariableInfo], tree: Tree): ListBuffer[VariableInfo] = {
      def seenName(name: Name) = buf exists (_.name == name)
      def add(name: Name, t: Tree): ListBuffer[VariableInfo] =
        if (seenName(name)) buf else buf += VariableInfo(name, t, namePos(tree, name))

      tree match {
        case Bind(nme.WILDCARD, _) =>
          foldOver(buf, tree)
        case Bind(name, Typed(tree1, tpt)) if !treeInfo.mayBeTypePat(tpt) =>
          apply(add(name, tpt), tree1)
        case Bind(name, tree1)              =>
          apply(add(name, TypeTree()), tree1)
        case _ =>
          foldOver(buf, tree)
      }
    }
  }

  /** Returns list of all pattern variables, possibly with their types,
   *  without duplicates
   */
  private def getVariables(tree: Tree): List[VariableInfo] =
    getVars(new ListBuffer[VariableInfo], tree).toList

  def byNameApplication(tpe: Tree)(implicit cpos: Position): Tree =
    AppliedTypeTree(scalaDot(tpnme.BYNAME_PARAM_CLASS), List(tpe))
  def repeatedApplication(tpe: Tree)(implicit cpos: Position): Tree =
    AppliedTypeTree(scalaDot(tpnme.REPEATED_PARAM_CLASS), List(tpe))

  private def makeTuple(trees: List[Tree])(implicit cpos: Position): Tree = {
    def mkPair(t1: Tree, t2: Tree) = Pair(t1, t2)(Position(t1.pos.start, cpos.end))
    trees reduce mkPair
  }

  def stripParens(t: Tree) = t match {
    case Parens(ts) => makeTuple(ts)(t.pos)
    case _ => t
  }

  def makeSelfDef(name: TermName, tpt: Tree)(implicit cpos: Position): ValDef =
    ValDef(Modifiers(Private), name, tpt, EmptyTree())

  /** If tree is a variable pattern, return Some("its name and type").
   *  Otherwise return none */
  private def matchVarPattern(tree: Tree): Option[(Name, Tree)] = {
    def wildType(t: Tree): Option[Tree] = t match {
      case Ident(x) if x.toTermName == nme.WILDCARD             => Some(TypeTree())
      case Typed(Ident(x), tpt) if x.toTermName == nme.WILDCARD => Some(tpt)
      case _                                                    => None
    }
    tree match {
      case Ident(name)             => Some((name, TypeTree()))
      case Bind(name, body)        => wildType(body) map (x => (name, x))
      case Typed(Ident(name), tpt) => Some((name, tpt))
      case _                       => None
    }
  }

  /** Create tree representing (unencoded) binary operation expression or pattern. */
  def makeBinop(isExpr: Boolean, left: Tree, op: TermName, right: Tree, opPos: Position): Tree = {
    def mkNamed(args: List[Tree]) =
      if (isExpr) args map {
        case arg @ Assign(Ident(name), rhs) => NamedArg(name, rhs)(arg.pos)
        case arg => arg
      } else args
    val arguments = right match {
      case Parens(args) => mkNamed(args)
      case _ => right :: Nil
    }
    if (isExpr) {
      if (treeInfo.isLeftAssoc(op)) {
        Apply(Select(stripParens(left), op.encode)(opPos), arguments)
      } else {
        val x = ctx.freshName().toTermName
        Block(
          List(ValDef(Modifiers(Synthetic), x, TypeTree(), stripParens(left))),
          Apply(Select(stripParens(right), op.encode)(opPos), List(Ident(x)(left.pos))))
      }
    } else {
      Apply(Ident(op.encode)(opPos), stripParens(left) :: arguments)
    }
  }

  /** Creates a tree representing new Object { stats }.
   *  To make sure an anonymous subclass of Object is created,
   *  if there are no stats, a () is added.
   */
  def makeAnonymousNew(stats: List[Tree])(implicit cpos: Position): Tree = {
    val stats1 = if (stats.isEmpty) Literal(Constant(())) :: Nil else stats
    makeNew(Nil, EmptyValDef(), stats1)
  }

  /** tpt.<init> */
  def SelectConstructor(tpt: Tree)(implicit cpos: Position): Tree =
    Select(tpt, nme.CONSTRUCTOR)

  def splitArgss(constr: Tree, outerArgss: List[List[Tree]]): (Tree, List[List[Tree]]) = constr match {
    case Apply(tree, args) => splitArgss(tree, args :: outerArgss)
    case _ => (constr, if (outerArgss.isEmpty) ListOfNil else outerArgss)
  }

  /** new tpt(argss_1)...(argss_n)
   *  @param npos the position spanning <new tpt>, without any arguments
   */
  def makeNew(parentConstr: Tree, npos: Position) = {
    val (tpt, argss1) = splitArgss(parentConstr, Nil)
    (SelectConstructor(tpt)(npos) /: argss1)(Apply(_, _))
  }

  /** Create positioned tree representing an object creation <new parents { self => stats }
   *  @param pos  the position of the new, focus should be the first parent's start.
   */
  def makeNew(parents: List[Tree], self: ValDef, stats: List[Tree])(implicit cpos: Position): Tree = {
    val newPos = Position(cpos.start, cpos.point)
    val clsPos = Position(cpos.point, cpos.end)
    if (parents.isEmpty)
      makeNew(List(scalaAnyRefConstr(cpos.startPos)), self, stats)
    else if (parents.tail.isEmpty && stats.isEmpty)
      makeNew(parents.head, newPos)
    else {
      val x = tpnme.ANON_CLASS
      val nu = makeNew(Ident(x)(newPos), newPos)
      val clsDef = {
        implicit val cpos = clsPos
        ClassDef(Modifiers(Final), x, Nil, Template(parents, self, stats))
      }
      Block(clsDef, nu)
    }
  }

  /** Create a tree representing an assignment <lhs = rhs> */
  def makeAssign(lhs: Tree, rhs: Tree): Tree = lhs match {
    case Apply(fn, args) =>
      Apply(Select(fn, nme.update), args :+ rhs)
    case _ =>
      Assign(lhs, rhs)
  }

  /** A type tree corresponding to (possibly unary) intersection type
  def makeIntersectionTypeTree(tps: List[Tree]): Tree =
    if (tps.tail.isEmpty) tps.head
    else CompoundTypeTree(Template(tps, emptyValDef, Nil))*/

  private def labelDefAndCall(lname: TermName, rhs: Tree, call: Tree)(implicit cpos: Position) = {
    val ldef = DefDef(Modifiers(Label), lname, Nil, ListOfNil, TypeTree(), rhs)
    Block(ldef, call)
  }

  private def labelCall(lname: TermName)(implicit cpos: Position): Apply =
    Apply(Ident(lname), Nil)

  /** Create tree representing a while loop */
  def makeWhile(lname: TermName, cond: Tree, body: Tree)(implicit cpos: Position): Tree = {
    val continu = labelCall(lname)((cond.pos union body.pos).endPos)
    val rhs = {
      implicit val cpos = NoPosition
      If(cond, Block(body, continu), Literal(Constant())(continu.pos))
    }
    labelDefAndCall(lname, rhs, continu)
  }

  /** Create tree representing a do-while loop */
  def makeDoWhile(lname: TermName, body: Tree, cond: Tree)(implicit cpos: Position): Tree = {
    val continu = labelCall(lname)((cond.pos union body.pos).endPos)
    val rhs = Block(body, If(cond, continu, Literal(Constant())(continu.pos)))
    labelDefAndCall(lname, rhs, continu)
  }

  /** Create block of statements `stats`  */
  def makeBlock(stats: List[Tree])(implicit cpos: Position): Tree =
    if (stats.isEmpty) Literal(Constant())
    else if (!stats.last.isTerm) Block(stats, Literal(Constant())(cpos.endPos))
    else if (stats.length == 1) stats.head
    else Block(stats.init, stats.last)

  def makeFilter(tree: Tree, condition: Tree, canDrop: Boolean): Tree = {
    val cases = List(
      CaseDef(condition, EmptyTree(), Literal(Constant(true))),
      CaseDef(Ident(nme.WILDCARD), EmptyTree(), Literal(Constant(false)))
    )
    val matchTree = makeVisitor(cases, checkExhaustive = false, canDrop)
    locally {
      implicit val cpos = tree.pos
      Apply(Select(tree, nme.withFilter), matchTree :: Nil)
    }
  }

  /** Create tree for for-comprehension generator <pat <- rhs> or <pat = rhs> */
  def makeGenerator(pat: Tree, valeq: Boolean, rhs: Tree)(implicit cpos: Position): Enumerator = {
    val pat1 = patvarTransformer.transform(pat)
    if (valeq) ValEq(pat1, rhs)(cpos)
    else ValFrom(pat1, makeFilter(rhs, pat1, canDrop = true))(cpos)
  }

  def makeParam(pname: TermName, tpe: Tree)(implicit cpos: Position) =
    ValDef(Modifiers(Param), pname, tpe, EmptyTree())

  def makeSyntheticParam(pname: TermName)(implicit cpos: Position) =
    ValDef(Modifiers(SyntheticTermParam), pname, TypeTree(), EmptyTree())
/*
  def makeSyntheticTypeParam(pname: TypeName, bounds: Tree) =
    TypeDef(Modifiers(DEFERRED | SYNTHETIC), pname, Nil, bounds)
*/
  abstract class Enumerator { def pos: Position }
  case class ValFrom(pat: Tree, rhs: Tree)(implicit cpos: Position) extends Enumerator {
    val pos = cpos union pat.pos union rhs.pos
  }
  case class ValEq(pat: Tree, rhs: Tree)(implicit cpos: Position) extends Enumerator {
    val pos = cpos union pat.pos union rhs.pos
  }
  case class Filter(test: Tree)(implicit cpos: Position) extends Enumerator {
    val pos = cpos union test.pos
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
  private def makeFor(mapName: TermName, flatMapName: TermName, enums: List[Enumerator], body: Tree): Tree = {

    /** make a closure pat => body.
     *  The closure is assigned a transparent position with the point at pos.point and
     *  the limits given by pat and body.
     */
    def makeClosure(pat: Tree, body: Tree)(implicit cpos: Position): Tree =
      matchVarPattern(pat) match {
        case Some((name, tpt)) =>
          Function(ValDef(Modifiers(Param), name.toTermName, tpt, EmptyTree())(pat.pos), body)
        case None =>
          makeVisitor(List(CaseDef(pat, EmptyTree(), body)), checkExhaustive = false)
      }

    /** Make an application  qual.meth(pat => body) positioned at `pos`.
     */
    def makeCombination(meth: TermName, qual: Tree, pat: Tree, body: Tree)(implicit cpos: Position): Tree =
      Apply(Select(qual, meth)(NoPosition), makeClosure(pat, body))

    /** Optionally, if pattern is a `Bind`, the bound name, otherwise None.
     */
    def patternVar(pat: Tree): Option[Name] = pat match {
      case Bind(name, _) => Some(name)
      case _ => None
    }

    /** If `pat` is not yet a `Bind` wrap it in one with a fresh name
     */
    def makeBind(pat: Tree): Tree = pat match {
      case Bind(_, _) => pat
      case _ => Bind(ctx.freshName().toTermName, pat)
    }

    /** A reference to the name bound in Bind `pat`.
     */
    def makeValue(pat: Tree): Tree = pat match {
      case Bind(name, _) => Ident(name)(pat.pos.focus)
    }

   enums match {
      case (enum @ ValFrom(pat, rhs)) :: Nil =>
        makeCombination(mapName, rhs, pat, body)(enum.pos)
      case ValFrom(pat, rhs) :: (rest @ (ValFrom( _, _) :: _)) =>
        makeCombination(flatMapName, rhs, pat,
                        makeFor(mapName, flatMapName, rest, body))
      case (enum @ ValFrom(pat, rhs)) :: Filter(test) :: rest =>
        makeFor(mapName, flatMapName,
                ValFrom(pat, makeCombination(nme.withFilter, rhs, pat, test))(enum.pos) :: rest,
                body)
      case (enum @ ValFrom(pat, rhs)) :: rest =>
        val (valeqs, rest1) = rest.span(_.isInstanceOf[ValEq])
        assert(!valeqs.isEmpty)
        val pats = valeqs map { case ValEq(pat, _) => pat }
        val rhss = valeqs map { case ValEq(_, rhs) => rhs }
        val defpat1 = makeBind(pat)
        val defpats = pats map makeBind
        val pdefs = (defpats, rhss).zipped flatMap makePatDef
        val ids = (defpat1 :: defpats) map makeValue
        val rhs1 = makeForYield(ValFrom(defpat1, rhs) :: Nil, Block(pdefs, makeTuple(ids)))
        val allpats = pat :: pats
        val vfrom1 = ValFrom(makeTuple(allpats), rhs1)(enum.pos)
        makeFor(mapName, flatMapName, vfrom1 :: rest1, body)
      case _ =>
        EmptyTree() //may happen for erroneous input
    }
  }

  /** Create tree for for-do comprehension <for (enums) body> */
  def makeFor(enums: List[Enumerator], body: Tree): Tree =
    makeFor(nme.foreach, nme.foreach, enums, body)

  /** Create tree for for-yield comprehension <for (enums) yield body> */
  def makeForYield(enums: List[Enumerator], body: Tree): Tree =
    makeFor(nme.map, nme.flatMap, enums, body)

  /** Create tree for a pattern alternative */
  def makeAlternative(ts: List[Tree]): Tree = {
    def alternatives(t: Tree): List[Tree] = t match {
      case Alternative(ts)  => ts
      case _                => List(t)
    }
    Alternative(ts flatMap alternatives)
  }

  def mkAnnotated(cls: Symbol, tree: Tree) =
    Annotated(TypedSplice(tpd.New(cls.typeConstructor)), tree)

  /** Create visitor <x => x match cases> */
  def makeVisitor(cases: List[CaseDef], checkExhaustive: Boolean, canDrop: Boolean = false): Tree = {
    val x   = ctx.freshName().toTermName
    val id  = Ident(x)
    val sel =
      if (canDrop) mkAnnotated(defn.DropIfRedundantAnnot, id)
      else if (!checkExhaustive) mkAnnotated(defn.UncheckedAnnot, id)
      else id
    Function(List(makeSyntheticParam(x)), Match(sel, cases))
  }

  /** Create tree for case definition <case pat if guard => rhs> */
  def makeCaseDef(pat: Tree, guard: Tree, rhs: Tree): CaseDef =
    CaseDef(patvarTransformer.transform(pat), guard, rhs)

  /** Creates tree representing:
   *    { case x: Throwable =>
   *        val catchFn = catchExpr
   *        if (catchFn isDefinedAt x) catchFn(x) else throw x
   *    }
   */
  def makeCatchFromExpr(catchExpr: Tree): CaseDef = {
    implicit val cpos = catchExpr.pos.startPos
    val binder   = ctx.freshName("x").toTermName
    val pat      = Bind(binder, Typed(Ident(nme.WILDCARD), Ident(tpnme.Throwable)))
    val catchDef = ValDef(Modifiers(), ctx.freshName("catchExpr").toTermName, TypeTree(), catchExpr)
    val catchFn  = Ident(catchDef.name)
    val cond     = Apply(Select(catchFn, nme.isDefinedAt), Ident(binder))
    val app      = Apply(Select(catchFn, nme.apply), List(Ident(binder)))
    val thro     = Throw(Ident(binder))
    val body     = Block(catchDef, If(cond, app, thro))
    makeCaseDef(pat, EmptyTree(), body)
  }

  /** Create tree for pattern definition <val pat0 = rhs> */
  def makePatDef(pat: Tree, rhs: Tree): List[Tree] =
    makePatDef(Modifiers(), pat, rhs)

  /** Create tree for pattern definition <mods val pat0 = rhs> */
  def makePatDef(mods: Modifiers, pat: Tree, rhs: Tree, varsArePatterns: Boolean = false): List[Tree] = matchVarPattern(pat) match {
    case Some((name, tpt)) if varsArePatterns =>
      ValDef(mods, name.toTermName, tpt, rhs) :: Nil

    case _ =>
      //  in case there is exactly one variable x_1 in pattern
      //  val/var p = e  ==>  val/var x_1 = e.match (case p => (x_1))
      //
      //  in case there are zero or more than one variables in pattern
      //  val/var p = e  ==>  private synthetic val t$ = e.match (case p => (x_1, ..., x_N))
      //                  val/var x_1 = t$._1
      //                  ...
      //                  val/var x_N = t$._N

      val rhsUnchecked = mkAnnotated(defn.UncheckedAnnot, rhs)

      // TODO: clean this up -- there is too much information packed into makePatDef's `pat` argument
      // when it's a simple identifier (case Some((name, tpt)) -- above),
      // pat should have the type ascription that was specified by the user
      // however, in `case None` (here), we must be careful not to generate illegal pattern trees (such as `(a, b): Tuple2[Int, String]`)
      // i.e., this must hold: pat1 match { case Typed(expr, tp) => assert(expr.isInstanceOf[Ident]) case _ => }
      // if we encounter such an erroneous pattern, we strip off the type ascription from pat and propagate the type information to rhs
      val (pat1, rhs1) = patvarTransformer.transform(pat) match {
        // move the Typed ascription to the rhs
        case Typed(expr, tpt) if !expr.isInstanceOf[Ident] =>
          val rhsTypedUnchecked =
            if (tpt.isEmpty) rhsUnchecked else Typed(rhsUnchecked, tpt)
          (expr, rhsTypedUnchecked)
        case ok =>
          (ok, rhsUnchecked)
      }
      val vars = getVariables(pat1)
      val ids = vars map (v => Ident(v.name)(v.pos))
      val caseDef = CaseDef(pat1, EmptyTree(), makeTuple(ids))
      val matchExpr = Match(rhs1, caseDef :: Nil)
      vars match {
        case List(VariableInfo(vname, tpt, pos)) =>
          ValDef(mods, vname.toTermName, tpt, matchExpr) :: Nil
        case _ =>
          val tmpName = ctx.freshName().toTermName
          val patMods = Modifiers(PrivateLocal | Synthetic | (mods.flags & Lazy))
          val firstDef = ValDef(patMods, tmpName, TypeTree(), matchExpr)
          val restDefs = for {
            (VariableInfo(vname, tpt, pos), n) <- vars.zipWithIndex
          } yield {
            val rhs = {
              implicit val cpos = pos.focus
              Select(Ident(tmpName), ("_" + n).toTermName)
            }
            ValDef(mods, vname.toTermName, tpt, rhs)(pos)
          }
          firstDef :: restDefs
      }
  }

  /** Create a tree representing the function type (argtpes) => restpe */
  def makeFunctionTypeTree(argtpes: List[Tree], restpe: Tree): Tree =
    AppliedTypeTree(scalaDot(("Function" + argtpes.length).toTypeName), argtpes ::: List(restpe))

  /** Append implicit parameter section if `contextBounds` nonempty */
  def addEvidenceParams(owner: Name, vparamss: List[List[ValDef]], contextBounds: List[Tree]): List[List[ValDef]] = {
    if (contextBounds.isEmpty) vparamss
    else {
      val mods = Modifiers(if (owner.isTypeName) PrivateLocal | ParamAccessor else Param)
      val evidenceParams = for (tpt <- contextBounds) yield {
        val pname = ctx.freshName(nme.EVIDENCE_PARAM_PREFIX.toString).toTermName
        ValDef(mods | Implicit | Synthetic, pname, tpt, EmptyTree())
      }
      vparamss.reverse match {
        case (vparams @ (vparam :: _)) :: _ if vparam.mods is Implicit =>
          vparamss.init :+ (evidenceParams ++ vparams)
        case _ =>
          vparamss :+ evidenceParams
      }
    }
  }
}
