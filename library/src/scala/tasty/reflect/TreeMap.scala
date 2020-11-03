package scala.tasty
package reflect

/** TASTy Reflect tree map.
 *
 *  Usage:
 *  ```
 *  class MyTreeMap[R <: scala.tasty.Reflection & Singleton](val reflect: R)
 *      extends scala.tasty.reflect.TreeMap {
 *    import reflect._
 *    override def transformTree(tree: Tree)(using ctx: Context): Tree = ...
 *  }
 *  ```
 */
trait TreeMap {

  val reflect: Reflection
  import reflect._

  def transformTree(tree: Tree)(using ctx: Context): Tree = {
    tree match {
      case tree: PackageClause =>
        PackageClause.copy(tree)(transformTerm(tree.pid).asInstanceOf[Ref], transformTrees(tree.stats)(using tree.symbol.localContext))
      case tree: Import =>
        Import.copy(tree)(transformTerm(tree.expr), tree.selectors)
      case tree: Statement =>
        transformStatement(tree)
      case tree: TypeTree => transformTypeTree(tree)
      case tree: TypeBoundsTree => tree // TODO traverse tree
      case tree: WildcardTypeTree => tree // TODO traverse tree
      case tree: CaseDef =>
        transformCaseDef(tree)
      case tree: TypeCaseDef =>
        transformTypeCaseDef(tree)
      case pattern: Bind =>
        Bind.copy(pattern)(pattern.name, pattern.pattern)
      case pattern: Unapply =>
        Unapply.copy(pattern)(transformTerm(pattern.fun), transformSubTrees(pattern.implicits), transformTrees(pattern.patterns))
      case pattern: Alternatives =>
        Alternatives.copy(pattern)(transformTrees(pattern.patterns))
    }
  }

  def transformStatement(tree: Statement)(using ctx: Context): Statement = {
    def localCtx(definition: Definition): Context = definition.symbol.localContext
    tree match {
      case tree: Term =>
        transformTerm(tree)
      case tree: ValDef =>
        val ctx = localCtx(tree)
        given Context = ctx
        val tpt1 = transformTypeTree(tree.tpt)
        val rhs1 = tree.rhs.map(x => transformTerm(x))
        ValDef.copy(tree)(tree.name, tpt1, rhs1)
      case tree: DefDef =>
        val ctx = localCtx(tree)
        given Context = ctx
        DefDef.copy(tree)(tree.name, transformSubTrees(tree.typeParams), tree.paramss mapConserve (transformSubTrees(_)), transformTypeTree(tree.returnTpt), tree.rhs.map(x => transformTerm(x)))
      case tree: TypeDef =>
        val ctx = localCtx(tree)
        given Context = ctx
        TypeDef.copy(tree)(tree.name, transformTree(tree.rhs))
      case tree: ClassDef =>
        ClassDef.copy(tree)(tree.name, tree.constructor, tree.parents, tree.derived, tree.self, tree.body)
      case tree: Import =>
        Import.copy(tree)(transformTerm(tree.expr), tree.selectors)
    }
  }

  def transformTerm(tree: Term)(using ctx: Context): Term = {
    tree match {
      case Ident(name) =>
        tree
      case Select(qualifier, name) =>
        Select.copy(tree)(transformTerm(qualifier), name)
      case This(qual) =>
        tree
      case Super(qual, mix) =>
        Super.copy(tree)(transformTerm(qual), mix)
      case Apply(fun, args) =>
        Apply.copy(tree)(transformTerm(fun), transformTerms(args))
      case TypeApply(fun, args) =>
        TypeApply.copy(tree)(transformTerm(fun), transformTypeTrees(args))
      case Literal(const) =>
        tree
      case New(tpt) =>
        New.copy(tree)(transformTypeTree(tpt))
      case Typed(expr, tpt) =>
        Typed.copy(tree)(transformTerm(expr), transformTypeTree(tpt))
      case tree: NamedArg =>
        NamedArg.copy(tree)(tree.name, transformTerm(tree.value))
      case Assign(lhs, rhs) =>
        Assign.copy(tree)(transformTerm(lhs), transformTerm(rhs))
      case Block(stats, expr) =>
        Block.copy(tree)(transformStats(stats), transformTerm(expr))
      case If(cond, thenp, elsep) =>
        If.copy(tree)(transformTerm(cond), transformTerm(thenp), transformTerm(elsep))
      case Closure(meth, tpt) =>
        Closure.copy(tree)(transformTerm(meth), tpt)
      case Match(selector, cases) =>
        Match.copy(tree)(transformTerm(selector), transformCaseDefs(cases))
      case Return(expr, from) =>
        Return.copy(tree)(transformTerm(expr), from)
      case While(cond, body) =>
        While.copy(tree)(transformTerm(cond), transformTerm(body))
      case Try(block, cases, finalizer) =>
        Try.copy(tree)(transformTerm(block), transformCaseDefs(cases), finalizer.map(x => transformTerm(x)))
      case Repeated(elems, elemtpt) =>
        Repeated.copy(tree)(transformTerms(elems), transformTypeTree(elemtpt))
      case Inlined(call, bindings, expansion) =>
        Inlined.copy(tree)(call, transformSubTrees(bindings), transformTerm(expansion)/*()call.symbol.localContext)*/)
    }
  }

  def transformTypeTree(tree: TypeTree)(using ctx: Context): TypeTree = tree match {
    case Inferred() => tree
    case tree: TypeIdent => tree
    case tree: TypeSelect =>
      TypeSelect.copy(tree)(tree.qualifier, tree.name)
    case tree: Projection =>
      Projection.copy(tree)(tree.qualifier, tree.name)
    case tree: Annotated =>
      Annotated.copy(tree)(tree.arg, tree.annotation)
    case tree: Singleton =>
      Singleton.copy(tree)(transformTerm(tree.ref))
    case tree: Refined =>
      Refined.copy(tree)(transformTypeTree(tree.tpt), transformTrees(tree.refinements).asInstanceOf[List[Definition]])
    case tree: Applied =>
      Applied.copy(tree)(transformTypeTree(tree.tpt), transformTrees(tree.args))
    case tree: MatchTypeTree =>
      MatchTypeTree.copy(tree)(tree.bound.map(b => transformTypeTree(b)), transformTypeTree(tree.selector), transformTypeCaseDefs(tree.cases))
    case tree: ByName =>
      ByName.copy(tree)(transformTypeTree(tree.result))
    case tree: LambdaTypeTree =>
      LambdaTypeTree.copy(tree)(transformSubTrees(tree.tparams), transformTree(tree.body))
    case tree: TypeBind =>
      TypeBind.copy(tree)(tree.name, tree.body)
    case tree: TypeBlock =>
      TypeBlock.copy(tree)(tree.aliases, tree.tpt)
  }

  def transformCaseDef(tree: CaseDef)(using ctx: Context): CaseDef = {
    CaseDef.copy(tree)(transformTree(tree.pattern), tree.guard.map(transformTerm), transformTerm(tree.rhs))
  }

  def transformTypeCaseDef(tree: TypeCaseDef)(using ctx: Context): TypeCaseDef = {
    TypeCaseDef.copy(tree)(transformTypeTree(tree.pattern), transformTypeTree(tree.rhs))
  }

  def transformStats(trees: List[Statement])(using ctx: Context): List[Statement] =
    trees mapConserve (transformStatement(_))

  def transformTrees(trees: List[Tree])(using ctx: Context): List[Tree] =
    trees mapConserve (transformTree(_))

  def transformTerms(trees: List[Term])(using ctx: Context): List[Term] =
    trees mapConserve (transformTerm(_))

  def transformTypeTrees(trees: List[TypeTree])(using ctx: Context): List[TypeTree] =
    trees mapConserve (transformTypeTree(_))

  def transformCaseDefs(trees: List[CaseDef])(using ctx: Context): List[CaseDef] =
    trees mapConserve (transformCaseDef(_))

  def transformTypeCaseDefs(trees: List[TypeCaseDef])(using ctx: Context): List[TypeCaseDef] =
    trees mapConserve (transformTypeCaseDef(_))

  def transformSubTrees[Tr <: Tree](trees: List[Tr])(using ctx: Context): List[Tr] =
    transformTrees(trees).asInstanceOf[List[Tr]]

}
