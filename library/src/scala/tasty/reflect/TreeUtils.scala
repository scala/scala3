package scala.tasty
package reflect

/** Tasty reflect case definition */
trait TreeUtils
    extends ReflectionCore
    with CaseDefOps
    with PatternOps
    with SymbolOps
    with TreeOps
    with TypeOrBoundsTreeOps {

  abstract class TreeAccumulator[X] {

    // Ties the knot of the traversal: call `foldOver(x, tree))` to dive in the `tree` node.
    def foldTree(x: X, tree: Tree)(implicit ctx: Context): X
    def foldTypeTree(x: X, tree: TypeOrBoundsTree)(implicit ctx: Context): X
    def foldCaseDef(x: X, tree: CaseDef)(implicit ctx: Context): X
    def foldPattern(x: X, tree: Pattern)(implicit ctx: Context): X

    def foldTrees(x: X, trees: Iterable[Tree])(implicit ctx: Context): X = (x /: trees)(foldTree)
    def foldTypeTrees(x: X, trees: Iterable[TypeOrBoundsTree])(implicit ctx: Context): X = (x /: trees)(foldTypeTree)
    def foldCaseDefs(x: X, trees: Iterable[CaseDef])(implicit ctx: Context): X = (x /: trees)(foldCaseDef)
    def foldPatterns(x: X, trees: Iterable[Pattern])(implicit ctx: Context): X = (x /: trees)(foldPattern)
    private def foldParents(x: X, trees: Iterable[TermOrTypeTree])(implicit ctx: Context): X = (x /: trees)(foldOverTermOrTypeTree)

    def foldOverTree(x: X, tree: Tree)(implicit ctx: Context): X = {
      def localCtx(definition: Definition): Context = definition.symbol.localContext
      tree match {
        case Term.Ident(_) =>
          x
        case Term.Select(qualifier, _, _) =>
          foldTree(x, qualifier)
        case Term.This(qual) =>
          x
        case Term.Super(qual, _) =>
          foldTree(x, qual)
        case Term.Apply(fun, args) =>
          foldTrees(foldTree(x, fun), args)
        case Term.TypeApply(fun, args) =>
          foldTypeTrees(foldTree(x, fun), args)
        case Term.Literal(const) =>
          x
        case Term.New(tpt) =>
          foldTypeTree(x, tpt)
        case Term.Typed(expr, tpt) =>
          foldTypeTree(foldTree(x, expr), tpt)
        case Term.NamedArg(_, arg) =>
          foldTree(x, arg)
        case Term.Assign(lhs, rhs) =>
          foldTree(foldTree(x, lhs), rhs)
        case Term.Block(stats, expr) =>
          foldTree(foldTrees(x, stats), expr)
        case Term.If(cond, thenp, elsep) =>
          foldTree(foldTree(foldTree(x, cond), thenp), elsep)
        case Term.Lambda(meth, tpt) =>
          val a = foldTree(x, meth)
          tpt.fold(a)(b => foldTypeTree(a, b))
        case Term.Match(selector, cases) =>
          foldCaseDefs(foldTree(x, selector), cases)
        case Term.Return(expr) =>
          foldTree(x, expr)
        case Term.Try(block, handler, finalizer) =>
          foldTrees(foldCaseDefs(foldTree(x, block), handler), finalizer)
        case Term.Repeated(elems) =>
          foldTrees(x, elems)
        case Term.Inlined(call, bindings, expansion) =>
          foldTree(foldTrees(x, bindings), expansion)
        case IsDefinition(vdef @ ValDef(_, tpt, rhs)) =>
          implicit val ctx = localCtx(vdef)
          foldTrees(foldTypeTree(x, tpt), rhs)
        case IsDefinition(ddef @ DefDef(_, tparams, vparamss, tpt, rhs)) =>
          implicit val ctx = localCtx(ddef)
          foldTrees(foldTypeTree((foldTrees(x, tparams) /: vparamss)(foldTrees), tpt), rhs)
        case IsDefinition(tdef @ TypeDef(_, rhs)) =>
          implicit val ctx = localCtx(tdef)
          foldTypeTree(x, rhs)
        case IsDefinition(cdef @ ClassDef(_, constr, parents, self, body)) =>
          implicit val ctx = localCtx(cdef)
          foldTrees(foldTrees(foldParents(foldTree(x, constr), parents), self), body)
        case Import(expr, selectors) =>
          foldTree(x, expr)
        case IsPackageClause(clause @ PackageClause(pid, stats)) =>
          foldTrees(foldTree(x, pid), stats)(clause.symbol.localContext)
      }
    }

    def foldOverTypeTree(x: X, tree: TypeOrBoundsTree)(implicit ctx: Context): X = tree match {
      case TypeTree.Synthetic() => x
      case TypeTree.Ident(_) => x
      case TypeTree.Select(qualifier, _) => foldTree(x, qualifier)
      case TypeTree.Project(qualifier, _) => foldTypeTree(x, qualifier)
      case TypeTree.Singleton(ref) => foldTree(x, ref)
      case TypeTree.And(left, right) => foldTypeTree(foldTypeTree(x, left), right)
      case TypeTree.Or(left, right) => foldTypeTree(foldTypeTree(x, left), right)
      case TypeTree.Refined(tpt, refinements) => foldTrees(foldTypeTree(x, tpt), refinements)
      case TypeTree.Applied(tpt, args) => foldTypeTrees(foldTypeTree(x, tpt), args)
      case TypeTree.ByName(result) => foldTypeTree(x, result)
      case TypeTree.Annotated(arg, annot) => foldTree(foldTypeTree(x, arg), annot)
      case TypeBoundsTree(lo, hi) => foldTypeTree(foldTypeTree(x, lo), hi)
    }

    def foldOverCaseDef(x: X, tree: CaseDef)(implicit ctx: Context): X = tree match {
      case CaseDef(pat, guard, body) => foldTree(foldTrees(foldPattern(x, pat), guard), body)
    }

    def foldOverPattern(x: X, tree: Pattern)(implicit ctx: Context): X = tree match {
      case Pattern.Value(v) => foldTree(x, v)
      case Pattern.Bind(_, body) => foldPattern(x, body)
      case Pattern.Unapply(fun, implicits, patterns) => foldPatterns(foldTrees(foldTree(x, fun), implicits), patterns)
      case Pattern.Alternative(patterns) => foldPatterns(x, patterns)
      case Pattern.TypeTest(tpt) => foldTypeTree(x, tpt)
    }

    private def foldOverTermOrTypeTree(x: X, tree: TermOrTypeTree)(implicit ctx: Context): X = tree match {
      case IsTerm(termOrTypeTree) => foldOverTree(x, termOrTypeTree)
      case IsTypeTree(termOrTypeTree) => foldOverTypeTree(x, termOrTypeTree)
    }

  }

   abstract class TreeTraverser extends TreeAccumulator[Unit] {

    def traverseTree(tree: Tree)(implicit ctx: Context): Unit = traverseTreeChildren(tree)
    def traverseTypeTree(tree: TypeOrBoundsTree)(implicit ctx: Context): Unit = traverseTypeTreeChildren(tree)
    def traverseCaseDef(tree: CaseDef)(implicit ctx: Context): Unit = traverseCaseDefChildren(tree)
    def traversePattern(tree: Pattern)(implicit ctx: Context): Unit = traversePatternChildren(tree)

    def foldTree(x: Unit, tree: Tree)(implicit ctx: Context): Unit = traverseTree(tree)
    def foldTypeTree(x: Unit, tree: TypeOrBoundsTree)(implicit ctx: Context) = traverseTypeTree(tree)
    def foldCaseDef(x: Unit, tree: CaseDef)(implicit ctx: Context) = traverseCaseDef(tree)
    def foldPattern(x: Unit, tree: Pattern)(implicit ctx: Context) = traversePattern(tree)

    protected def traverseTreeChildren(tree: Tree)(implicit ctx: Context): Unit = foldOverTree((), tree)
    protected def traverseTypeTreeChildren(tree: TypeOrBoundsTree)(implicit ctx: Context): Unit = foldOverTypeTree((), tree)
    protected def traverseCaseDefChildren(tree: CaseDef)(implicit ctx: Context): Unit = foldOverCaseDef((), tree)
    protected def traversePatternChildren(tree: Pattern)(implicit ctx: Context): Unit = foldOverPattern((), tree)

  }

}
