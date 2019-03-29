package scala.tasty
package reflect

/** Tasty reflect case definition */
trait TreeUtils
    extends Core
    with PatternOps
    with SymbolOps
    with TreeOps {

  abstract class TreeAccumulator[X] {

    // Ties the knot of the traversal: call `foldOver(x, tree))` to dive in the `tree` node.
    def foldTree(x: X, tree: Tree)(implicit ctx: Context): X
    def foldPattern(x: X, tree: Pattern)(implicit ctx: Context): X

    def foldTrees(x: X, trees: Iterable[Tree])(implicit ctx: Context): X = (x /: trees)(foldTree)
    def foldPatterns(x: X, trees: Iterable[Pattern])(implicit ctx: Context): X = (x /: trees)(foldPattern)

    def foldOverTree(x: X, tree: Tree)(implicit ctx: Context): X = {
      def localCtx(definition: Definition): Context = definition.symbol.localContext
      tree match {
        case Term.Ident(_) =>
          x
        case Term.Select(qualifier, _) =>
          foldTree(x, qualifier)
        case Term.This(qual) =>
          x
        case Term.Super(qual, _) =>
          foldTree(x, qual)
        case Term.Apply(fun, args) =>
          foldTrees(foldTree(x, fun), args)
        case Term.TypeApply(fun, args) =>
          foldTrees(foldTree(x, fun), args)
        case Term.Literal(const) =>
          x
        case Term.New(tpt) =>
          foldTree(x, tpt)
        case Term.Typed(expr, tpt) =>
          foldTree(foldTree(x, expr), tpt)
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
          tpt.fold(a)(b => foldTree(a, b))
        case Term.Match(selector, cases) =>
          foldTrees(foldTree(x, selector), cases)
        case Term.Return(expr) =>
          foldTree(x, expr)
        case Term.Try(block, handler, finalizer) =>
          foldTrees(foldTrees(foldTree(x, block), handler), finalizer)
        case Term.Repeated(elems, elemtpt) =>
          foldTrees(foldTree(x, elemtpt), elems)
        case Term.Inlined(call, bindings, expansion) =>
          foldTree(foldTrees(x, bindings), expansion)
        case IsDefinition(vdef @ ValDef(_, tpt, rhs)) =>
          implicit val ctx = localCtx(vdef)
          foldTrees(foldTree(x, tpt), rhs)
        case IsDefinition(ddef @ DefDef(_, tparams, vparamss, tpt, rhs)) =>
          implicit val ctx = localCtx(ddef)
          foldTrees(foldTree((foldTrees(x, tparams) /: vparamss)(foldTrees), tpt), rhs)
        case IsDefinition(tdef @ TypeDef(_, rhs)) =>
          implicit val ctx = localCtx(tdef)
          foldTree(x, rhs)
        case IsDefinition(cdef @ ClassDef(_, constr, parents, derived, self, body)) =>
          implicit val ctx = localCtx(cdef)
          foldTrees(foldTrees(foldTrees(foldTrees(foldTree(x, constr), parents), derived), self), body)
        case Import(_, expr, _) =>
          foldTree(x, expr)
        case IsPackageClause(clause @ PackageClause(pid, stats)) =>
          foldTrees(foldTree(x, pid), stats)(clause.symbol.localContext)
        case TypeTree.Inferred() => x
        case TypeTree.TypeIdent(_) => x
        case TypeTree.TypeSelect(qualifier, _) => foldTree(x, qualifier)
        case TypeTree.Projection(qualifier, _) => foldTree(x, qualifier)
        case TypeTree.Singleton(ref) => foldTree(x, ref)
        case TypeTree.Refined(tpt, refinements) => foldTrees(foldTree(x, tpt), refinements)
        case TypeTree.Applied(tpt, args) => foldTrees(foldTree(x, tpt), args)
        case TypeTree.ByName(result) => foldTree(x, result)
        case TypeTree.Annotated(arg, annot) => foldTree(foldTree(x, arg), annot)
        case TypeTree.LambdaTypeTree(typedefs, arg) => foldTree(foldTrees(x, typedefs), arg)
        case TypeTree.TypeBind(_, tbt) => foldTree(x, tbt)
        case TypeTree.TypeBlock(typedefs, tpt) => foldTree(foldTrees(x, typedefs), tpt)
        case TypeTree.MatchType(boundopt, selector, cases) =>
          foldTrees(foldTree(boundopt.fold(x)(foldTree(x, _)), selector), cases)
        case WildcardTypeTree() => x
        case TypeBoundsTree(lo, hi) => foldTree(foldTree(x, lo), hi)
        case CaseDef(pat, guard, body) => foldTree(foldTrees(foldPattern(x, pat), guard), body)
        case TypeCaseDef(pat, body) => foldTree(foldTree(x, pat), body)
      }
    }

    def foldOverPattern(x: X, tree: Pattern)(implicit ctx: Context): X = tree match {
      case Pattern.Value(v) => foldTree(x, v)
      case Pattern.Bind(_, body) => foldPattern(x, body)
      case Pattern.Unapply(fun, implicits, patterns) => foldPatterns(foldTrees(foldTree(x, fun), implicits), patterns)
      case Pattern.Alternatives(patterns) => foldPatterns(x, patterns)
      case Pattern.TypeTest(tpt) => foldTree(x, tpt)
    }

  }

  abstract class TreeTraverser extends TreeAccumulator[Unit] {

    def traverseTree(tree: Tree)(implicit ctx: Context): Unit = traverseTreeChildren(tree)
    def traversePattern(tree: Pattern)(implicit ctx: Context): Unit = traversePatternChildren(tree)

    def foldTree(x: Unit, tree: Tree)(implicit ctx: Context): Unit = traverseTree(tree)
    def foldPattern(x: Unit, tree: Pattern)(implicit ctx: Context) = traversePattern(tree)

    protected def traverseTreeChildren(tree: Tree)(implicit ctx: Context): Unit = foldOverTree((), tree)
    protected def traversePatternChildren(tree: Pattern)(implicit ctx: Context): Unit = foldOverPattern((), tree)

  }

  abstract class TreeMap { self =>

    def transformTree(tree: Tree)(implicit ctx: Context): Tree = {
      tree match {
        case IsPackageClause(tree) =>
          PackageClause.copy(tree)(transformTerm(tree.pid).asInstanceOf[Term.Ref], transformTrees(tree.stats)(tree.symbol.localContext))
        case IsImport(tree) =>
          Import.copy(tree)(tree.importImplied, transformTerm(tree.expr), tree.selectors)
        case IsStatement(tree) =>
          transformStatement(tree)
        case IsTypeTree(tree) => transformTypeTree(tree)
        case IsTypeBoundsTree(tree) => tree // TODO traverse tree
        case IsWildcardTypeTree(tree) => tree // TODO traverse tree
        case IsCaseDef(tree) =>
          transformCaseDef(tree)
        case IsTypeCaseDef(tree) =>
          transformTypeCaseDef(tree)
      }
    }

    def transformStatement(tree: Statement)(implicit ctx: Context): Statement = {
      def localCtx(definition: Definition): Context = definition.symbol.localContext
      tree match {
        case IsTerm(tree) =>
          transformTerm(tree)
        case IsValDef(tree) =>
          implicit val ctx = localCtx(tree)
          val tpt1 = transformTypeTree(tree.tpt)
          val rhs1 = tree.rhs.map(x => transformTerm(x))
          ValDef.copy(tree)(tree.name, tpt1, rhs1)
        case IsDefDef(tree) =>
          implicit val ctx = localCtx(tree)
          DefDef.copy(tree)(tree.name, transformSubTrees(tree.typeParams), tree.paramss mapConserve (transformSubTrees(_)), transformTypeTree(tree.returnTpt), tree.rhs.map(x => transformTerm(x)))
        case IsTypeDef(tree) =>
          implicit val ctx = localCtx(tree)
          TypeDef.copy(tree)(tree.name, transformTree(tree.rhs))
        case IsClassDef(tree) =>
          ClassDef.copy(tree)(tree.name, tree.constructor, tree.parents, tree.derived, tree.self, tree.body)
        case IsImport(tree) =>
          Import.copy(tree)(tree.importImplied, transformTerm(tree.expr), tree.selectors)
      }
    }

    def transformTerm(tree: Term)(implicit ctx: Context): Term = {
      tree match {
        case Term.Ident(name) =>
          tree
        case Term.Select(qualifier, name) =>
          Term.Select.copy(tree)(transformTerm(qualifier), name)
        case Term.This(qual) =>
          tree
        case Term.Super(qual, mix) =>
          Term.Super.copy(tree)(transformTerm(qual), mix)
        case Term.Apply(fun, args) =>
          Term.Apply.copy(tree)(transformTerm(fun), transformTerms(args))
        case Term.TypeApply(fun, args) =>
          Term.TypeApply.copy(tree)(transformTerm(fun), transformTypeTrees(args))
        case Term.Literal(const) =>
          tree
        case Term.New(tpt) =>
          Term.New.copy(tree)(transformTypeTree(tpt))
        case Term.Typed(expr, tpt) =>
          Term.Typed.copy(tree)(transformTerm(expr), transformTypeTree(tpt))
        case Term.IsNamedArg(tree) =>
          Term.NamedArg.copy(tree)(tree.name, transformTerm(tree.value))
        case Term.Assign(lhs, rhs) =>
          Term.Assign.copy(tree)(transformTerm(lhs), transformTerm(rhs))
        case Term.Block(stats, expr) =>
          Term.Block.copy(tree)(transformStats(stats), transformTerm(expr))
        case Term.If(cond, thenp, elsep) =>
          Term.If.copy(tree)(transformTerm(cond), transformTerm(thenp), transformTerm(elsep))
        case Term.Lambda(meth, tpt) =>
          Term.Lambda.copy(tree)(transformTerm(meth), tpt.map(x => transformTypeTree(x)))
        case Term.Match(selector, cases) =>
          Term.Match.copy(tree)(transformTerm(selector), transformCaseDefs(cases))
        case Term.Return(expr) =>
          Term.Return.copy(tree)(transformTerm(expr))
        case Term.While(cond, body) =>
          Term.While.copy(tree)(transformTerm(cond), transformTerm(body))
        case Term.Try(block, cases, finalizer) =>
          Term.Try.copy(tree)(transformTerm(block), transformCaseDefs(cases), finalizer.map(x => transformTerm(x)))
        case Term.Repeated(elems, elemtpt) =>
          Term.Repeated.copy(tree)(transformTerms(elems), transformTypeTree(elemtpt))
        case Term.Inlined(call, bindings, expansion) =>
          Term.Inlined.copy(tree)(call, transformSubTrees(bindings), transformTerm(expansion)/*()call.symbol.localContext)*/)
      }
    }

    def transformTypeTree(tree: TypeTree)(implicit ctx: Context): TypeTree = tree match {
      case TypeTree.Inferred() => tree
      case TypeTree.IsTypeIdent(tree) => tree
      case TypeTree.IsTypeSelect(tree) =>
        TypeTree.TypeSelect.copy(tree)(tree.qualifier, tree.name)
      case TypeTree.IsProjection(tree) =>
        TypeTree.Projection.copy(tree)(tree.qualifier, tree.name)
      case TypeTree.IsAnnotated(tree) =>
        TypeTree.Annotated.copy(tree)(tree.arg, tree.annotation)
      case TypeTree.IsSingleton(tree) =>
        TypeTree.Singleton.copy(tree)(transformTerm(tree.ref))
      case TypeTree.IsRefined(tree) =>
        TypeTree.Refined.copy(tree)(transformTypeTree(tree.tpt), transformTrees(tree.refinements).asInstanceOf[List[Definition]])
      case TypeTree.IsApplied(tree) =>
        TypeTree.Applied.copy(tree)(transformTypeTree(tree.tpt), transformTrees(tree.args))
      case TypeTree.IsMatchType(tree) =>
        TypeTree.MatchType.copy(tree)(tree.bound.map(b => transformTypeTree(b)), transformTypeTree(tree.selector), transformTypeCaseDefs(tree.cases))
      case TypeTree.IsByName(tree) =>
        TypeTree.ByName.copy(tree)(transformTypeTree(tree.result))
      case TypeTree.IsLambdaTypeTree(tree) =>
        TypeTree.LambdaTypeTree.copy(tree)(transformSubTrees(tree.tparams), transformTree(tree.body))(tree.symbol.localContext)
      case TypeTree.IsTypeBind(tree) =>
        TypeTree.TypeBind.copy(tree)(tree.name, tree.body)
      case TypeTree.IsTypeBlock(tree) =>
        TypeTree.TypeBlock.copy(tree)(tree.aliases, tree.tpt)
    }

    def transformCaseDef(tree: CaseDef)(implicit ctx: Context): CaseDef = {
      CaseDef.copy(tree)(transformPattern(tree.pattern), tree.guard.map(transformTerm), transformTerm(tree.rhs))
    }

    def transformTypeCaseDef(tree: TypeCaseDef)(implicit ctx: Context): TypeCaseDef = {
      TypeCaseDef.copy(tree)(transformTypeTree(tree.pattern), transformTypeTree(tree.rhs))
    }

    def transformPattern(pattern: Pattern)(implicit ctx: Context): Pattern = pattern match {
      case Pattern.Value(_) =>
        pattern
      case Pattern.IsTypeTest(pattern) =>
        Pattern.TypeTest.copy(pattern)(transformTypeTree(pattern.tpt))
      case Pattern.IsUnapply(pattern) =>
        Pattern.Unapply.copy(pattern)(transformTerm(pattern.fun), transformSubTrees(pattern.implicits), transformPatterns(pattern.patterns))
      case Pattern.IsAlternatives(pattern) =>
        Pattern.Alternatives.copy(pattern)(transformPatterns(pattern.patterns))
      case Pattern.IsBind(pattern) =>
        Pattern.Bind.copy(pattern)(pattern.name, transformPattern(pattern.pattern))
    }

    def transformStats(trees: List[Statement])(implicit ctx: Context): List[Statement] =
      trees mapConserve (transformStatement(_))

    def transformTrees(trees: List[Tree])(implicit ctx: Context): List[Tree] =
      trees mapConserve (transformTree(_))

    def transformTerms(trees: List[Term])(implicit ctx: Context): List[Term] =
      trees mapConserve (transformTerm(_))

    def transformTypeTrees(trees: List[TypeTree])(implicit ctx: Context): List[TypeTree] =
      trees mapConserve (transformTypeTree(_))

    def transformCaseDefs(trees: List[CaseDef])(implicit ctx: Context): List[CaseDef] =
      trees mapConserve (transformCaseDef(_))

    def transformTypeCaseDefs(trees: List[TypeCaseDef])(implicit ctx: Context): List[TypeCaseDef] =
      trees mapConserve (transformTypeCaseDef(_))

    def transformPatterns(trees: List[Pattern])(implicit ctx: Context): List[Pattern] =
      trees mapConserve (transformPattern(_))

    def transformSubTrees[Tr <: Tree](trees: List[Tr])(implicit ctx: Context): List[Tr] =
      transformTrees(trees).asInstanceOf[List[Tr]]

  }

}
