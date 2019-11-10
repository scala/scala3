package dotty.tools
package dotc
package typer

import core._
import Types._, Contexts._, Symbols._, Decorators._, Constants._
import annotation.tailrec
import StdNames.nme
import util.Property
import Names.Name
import util.Spans.Span
import Flags.Mutable

/** Operations for implementing a flow analysis for nullability */
object Nullables with
  import ast.tpd._

  /** A set of val or var references that are known to be not null, plus a set of
   *  variable references that are not known (anymore) to be null
   */
  case class NotNullInfo(asserted: Set[TermRef], retracted: Set[TermRef])
    assert((asserted & retracted).isEmpty)

    def isEmpty = this eq NotNullInfo.empty

    def retractedInfo = NotNullInfo(Set(), retracted)

    /** The sequential combination with another not-null info */
    def seq(that: NotNullInfo): NotNullInfo =
      if this.isEmpty then that
      else if that.isEmpty then this
      else NotNullInfo(
        this.asserted.union(that.asserted).diff(that.retracted),
        this.retracted.union(that.retracted).diff(that.asserted))

    /** The alternative path combination with another not-null info */
    def alt(that: NotNullInfo): NotNullInfo =
      NotNullInfo(this.asserted.intersect(that.asserted), this.retracted.union(that.retracted))

  object NotNullInfo with
    val empty = new NotNullInfo(Set(), Set())
    def apply(asserted: Set[TermRef], retracted: Set[TermRef]): NotNullInfo =
      if asserted.isEmpty && retracted.isEmpty then empty
      else new NotNullInfo(asserted, retracted)
  end NotNullInfo

  /** A pair of not-null sets, depending on whether a condition is `true` or `false` */
  case class NotNullConditional(ifTrue: Set[TermRef], ifFalse: Set[TermRef]) with
    def isEmpty = this eq NotNullConditional.empty

  object NotNullConditional with
    val empty = new NotNullConditional(Set(), Set())
    def apply(ifTrue: Set[TermRef], ifFalse: Set[TermRef]): NotNullConditional =
      if ifTrue.isEmpty && ifFalse.isEmpty then empty
      else new NotNullConditional(ifTrue, ifFalse)
  end NotNullConditional

  /** An attachment that represents conditional flow facts established
   *  by this tree, which represents a condition.
   */
  private[typer] val NNConditional = Property.StickyKey[NotNullConditional]

   /** An attachment that represents unconditional flow facts established
    *  by this tree.
    */
  private[typer] val NNInfo = Property.StickyKey[NotNullInfo]

  /** An extractor for null comparisons */
  object CompareNull with

    /** Matches one of
     *
     *    tree == null, tree eq null, null == tree, null eq tree
     *    tree != null, tree ne null, null != tree, null ne tree
     *
     *  The second boolean result is true for equality tests, false for inequality tests
     */
    def unapply(tree: Tree)(given Context): Option[(Tree, Boolean)] = tree match
      case Apply(Select(l, _), Literal(Constant(null)) :: Nil) =>
        testSym(tree.symbol, l)
      case Apply(Select(Literal(Constant(null)), _), r :: Nil) =>
        testSym(tree.symbol, r)
      case _ =>
        None

    private def testSym(sym: Symbol, operand: Tree)(given Context) =
      if sym == defn.Any_== || sym == defn.Object_eq then Some((operand, true))
      else if sym == defn.Any_!= || sym == defn.Object_ne then Some((operand, false))
      else None

  end CompareNull

  /** An extractor for null-trackable references */
  object TrackedRef
    def unapply(tree: Tree)(given Context): Option[TermRef] = tree.typeOpt match
      case ref: TermRef if isTracked(ref) => Some(ref)
      case _ => None
  end TrackedRef

  /** Is given reference tracked for nullability?
   *  This is the case if the reference is a path to an immutable val,
   *  or if it refers to a local mutable variable where all assignments
   *  to the variable are reachable.
   */
  def isTracked(ref: TermRef)(given Context) =
    ref.isStable
    || { val sym = ref.symbol
         sym.is(Mutable)
         && sym.owner.isTerm
         && sym.owner.enclosingMethod == curCtx.owner.enclosingMethod
         && sym.span.exists
         && curCtx.compilationUnit.trackedVarSpans.contains(sym.span.start)
//           .reporting(i"tracked? $sym ${sym.span} = $result")
       }

  def afterPatternContext(sel: Tree, pat: Tree)(given ctx: Context) = (sel, pat) match
    case (TrackedRef(ref), Literal(Constant(null))) => ctx.addNotNullRefs(Set(ref))
    case _ => ctx

  def caseContext(sel: Tree, pat: Tree)(given ctx: Context): Context = sel match
    case TrackedRef(ref) if matchesNotNull(pat) => ctx.addNotNullRefs(Set(ref))
    case _ => ctx

  private def matchesNotNull(pat: Tree)(given Context): Boolean = pat match
    case _: Typed | _: UnApply => true
    case Alternative(pats) => pats.forall(matchesNotNull)
    // TODO: Add constant pattern if the constant type is not nullable
    case _ => false

  given (infos: List[NotNullInfo])
    @tailRec
    def containsRef(ref: TermRef): Boolean = infos match
      case info :: infos1 =>
        if info.asserted.contains(ref) then true
        else if info.retracted.contains(ref) then false
        else containsRef(infos1)(ref)
      case _ =>
        false

    def extendWith(info: NotNullInfo) =
      if info.isEmpty
         || info.asserted.forall(infos.containsRef(_))
            && !info.retracted.exists(infos.containsRef(_))
      then infos
      else info :: infos

  given (tree: Tree)

    /* The `tree` with added nullability attachment */
    def withNotNullInfo(info: NotNullInfo): tree.type =
      if !info.isEmpty then tree.putAttachment(NNInfo, info)
      tree

    /* The nullability info of `tree` */
    def notNullInfo(given Context): NotNullInfo =
      stripInlined(tree).getAttachment(NNInfo) match
        case Some(info) if !curCtx.erasedTypes => info
        case _ => NotNullInfo.empty

    /* The nullability info of `tree`, assuming it is a condition that evaluates to `c` */
    def notNullInfoIf(c: Boolean)(given Context): NotNullInfo =
      val cond = tree.notNullConditional
      if cond.isEmpty then tree.notNullInfo
      else tree.notNullInfo.seq(NotNullInfo(if c then cond.ifTrue else cond.ifFalse, Set()))

    /** The paths that are known to be not null if the condition represented
     *  by `tree` yields `true` or `false`. Two empty sets if `tree` is not
     *  a condition.
     */
    def notNullConditional(given Context): NotNullConditional =
      stripBlock(tree).getAttachment(NNConditional) match
        case Some(cond) if !curCtx.erasedTypes => cond
        case _ => NotNullConditional.empty

    /** The current context augmented with nullability information of `tree` */
    def nullableContext(given Context): Context =
      val info = tree.notNullInfo
      if info.isEmpty then curCtx else curCtx.addNotNullInfo(info)

    /** The current context augmented with nullability information,
     *  assuming the result of the condition represented by `tree` is the same as
     *  the value of `c`.
     */
    def nullableContextIf(c: Boolean)(given Context): Context =
      val info = tree.notNullInfoIf(c)
      if info.isEmpty then curCtx else curCtx.addNotNullInfo(info)

    /** The context to use for the arguments of the function represented by `tree`.
     *  This is the current context, augmented with nullability information
     *  of the left argument, if the application is a boolean `&&` or `||`.
     */
    def nullableInArgContext(given Context): Context = tree match
      case Select(x, _) if !curCtx.erasedTypes =>
        if tree.symbol == defn.Boolean_&& then x.nullableContextIf(true)
        else if tree.symbol == defn.Boolean_|| then x.nullableContextIf(false)
        else curCtx
      case _ => curCtx

    /** The `tree` augmented with nullability information in an attachment.
     *  The following operations lead to nullability info being recorded:
     *
     *  1. Null tests using `==`, `!=`, `eq`, `ne`, if the compared entity is
     *     a path (i.e. a stable TermRef)
     *  2. Boolean &&, ||, !
     */
    def computeNullable()(given Context): tree.type =
      def setConditional(ifTrue: Set[TermRef], ifFalse: Set[TermRef]) =
        tree.putAttachment(NNConditional, NotNullConditional(ifTrue, ifFalse))
      if !curCtx.erasedTypes && analyzedOps.contains(tree.symbol.name.toTermName) then
        tree match
          case CompareNull(TrackedRef(ref), testEqual) =>
            if testEqual then setConditional(Set(), Set(ref))
            else setConditional(Set(ref), Set())
          case Apply(Select(x, _), y :: Nil) =>
            val xc = x.notNullConditional
            val yc = y.notNullConditional
            if !(xc.isEmpty && yc.isEmpty) then
              if tree.symbol == defn.Boolean_&& then
                setConditional(xc.ifTrue | yc.ifTrue, xc.ifFalse & yc.ifFalse)
              else if tree.symbol == defn.Boolean_|| then
                setConditional(xc.ifTrue & yc.ifTrue, xc.ifFalse | yc.ifFalse)
          case Select(x, _) if tree.symbol == defn.Boolean_! =>
            val xc = x.notNullConditional
            if !xc.isEmpty then
              setConditional(xc.ifFalse, xc.ifTrue)
          case _ =>
      tree

    /** Compute nullability information for this tree and all its subtrees */
    def computeNullableDeeply()(given Context): Unit =
      new TreeTraverser {
        def traverse(tree: Tree)(implicit ctx: Context) =
          traverseChildren(tree)
          tree.computeNullable()
      }.traverse(tree)

  given (tree: Assign)
    def computeAssignNullable()(given Context): tree.type = tree.lhs match
      case TrackedRef(ref) =>
        tree.withNotNullInfo(NotNullInfo(Set(), Set(ref))) // TODO: refine with nullability type info
      case _ => tree

  private val analyzedOps = Set(nme.EQ, nme.NE, nme.eq, nme.ne, nme.ZAND, nme.ZOR, nme.UNARY_!)

  /** The name offsets of all local mutable variables in the current compilation unit
   *  that have only reachable assignments. An assignment is reachable if the
   *  path of tree nodes between the block enclosing the variable declaration to
   *  the assignment consists only of if-expressions, while-expressions, block-expressions
   *  and type-ascriptions. Only reachable assignments are handled correctly in the
   *  nullability analysis. Therefore, variables with unreachable assignments can
   *  be assumed to be not-null only if their type asserts it.
   */
  def trackedVarSpans(given Context): Set[Int] =
    import ast.untpd._
    object populate extends UntypedTreeTraverser with

      /** The name offsets of variables that are tracked */
      var tracked: Set[Int] = Set.empty
      /** The names of candidate variables in scope that might be tracked */
      var candidates: Set[Name] = Set.empty
      /** An assignment to a variable that's not in reachable makes the variable ineligible for tracking */
      var reachable: Set[Name] = Set.empty

      def traverse(tree: Tree)(implicit ctx: Context) =
        val savedReachable = reachable
        tree match
          case Block(stats, expr) =>
            var shadowed: Set[Name] = Set.empty
            for case (stat: ValDef) <- stats if stat.mods.is(Mutable) do
              if candidates.contains(stat.name) then shadowed += stat.name
              else candidates += stat.name
              reachable += stat.name
            traverseChildren(tree)
            for case (stat: ValDef) <- stats if stat.mods.is(Mutable) do
              if candidates.contains(stat.name) then
                tracked += stat.nameSpan.start // candidates that survive until here are tracked
                candidates -= stat.name
            candidates ++= shadowed
          case Assign(Ident(name), rhs) =>
            if !reachable.contains(name) then candidates -= name // variable cannot be tracked
            traverseChildren(tree)
          case _: (If | WhileDo | Typed) =>
            traverseChildren(tree)      // assignments to candidate variables are OK here ...
          case _ =>
            reachable = Set.empty       // ... but not here
            traverseChildren(tree)
        reachable = savedReachable

    populate.traverse(curCtx.compilationUnit.untpdTree)
    populate.tracked
      .reporting(i"tracked vars: ${result.toList}%, %")
  end trackedVarSpans
end Nullables
