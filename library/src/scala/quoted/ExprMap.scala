package scala.quoted

import language.experimental.captureChecking

trait ExprMap:

  /** Maps an expression `e` with a type `T`.
   *  Requires a given `Type[T]` instance for staging and a `Quotes` instance
   *  for access to the reflection API.
   *
   *  @tparam T the type of the expression being transformed
   *  @param e the expression to transform
   */
  def transform[T](e: Expr[T])(using Type[T])(using Quotes): Expr[T]

  /** Maps the sub-expressions of an expression `e` with type `T`.
   *  Requires a given `Type[T]` instance for staging and a `Quotes` instance
   *  for access to the reflection API.
   *
   *  @tparam T the type of the expression whose children are transformed
   *  @param e the expression whose direct sub-expressions will be transformed via `transform`
   */
  def transformChildren[T](e: Expr[T])(using Type[T])(using Quotes): Expr[T] = {
    import quotes.reflect.*
    final class MapChildren() {

      def transformStatement(tree: Statement)(owner: Symbol): Statement = {
        tree match {
          case tree: Term =>
            transformTerm(tree, TypeRepr.of[Any])(owner)
          case tree: Definition =>
            transformDefinition(tree)(owner)
          case tree @ (_:Import | _:Export) =>
            tree
        }
      }

      def transformDefinition(tree: Definition)(owner: Symbol): Definition = {
        tree match {
          case tree: ValDef =>
            val owner = tree.symbol
            val rhs1 = tree.rhs.map(x => transformTerm(x, tree.tpt.tpe)(owner))
            ValDef.copy(tree)(tree.name, tree.tpt, rhs1)
          case tree: DefDef =>
            val owner = tree.symbol
            DefDef.copy(tree)(tree.name, tree.paramss, tree.returnTpt, tree.rhs.map(x => transformTerm(x, tree.returnTpt.tpe)(owner)))
          case tree: TypeDef =>
            tree
          case tree: ClassDef =>
            val newBody = transformStats(tree.body)(owner)
            ClassDef.copy(tree)(tree.name, tree.constructor, tree.parents, tree.self, newBody)
        }
      }

      def transformTermChildren(tree: Term, tpe: TypeRepr)(owner: Symbol): Term = tree match {
        case Ident(name) =>
          tree
        case Select(qualifier, name) =>
          Select.copy(tree)(transformTerm(qualifier, qualifier.tpe)(owner), name)
        case This(qual) =>
          tree
        case Super(qual, mix) =>
          tree
        case tree @ Apply(fun, args) =>
          val MethodType(_, tpes, _) = fun.tpe.widen: @unchecked
          val tpes1 = tpes.map {
            case ByNameType(tpe) => tpe
            case tpe => tpe
          }
          Apply.copy(tree)(transformTerm(fun, TypeRepr.of[Any])(owner), transformTerms(args, tpes1)(owner))
        case TypeApply(fun, args) =>
          TypeApply.copy(tree)(transformTerm(fun, TypeRepr.of[Any])(owner), args)
        case _: Literal =>
          tree
        case New(tpt) =>
          New.copy(tree)(transformTypeTree(tpt)(owner))
        case Typed(expr, tpt) =>
          val tp = tpt.tpe match
            case AppliedType(TypeRef(ThisType(TypeRef(NoPrefix(), "scala")), "<repeated>"), List(tp0: TypeRepr)) =>
              TypeRepr.of[Seq].appliedTo(tp0)
            case tp => tp
          Typed.copy(tree)(transformTerm(expr, tp)(owner), transformTypeTree(tpt)(owner))
        case tree: NamedArg =>
          NamedArg.copy(tree)(tree.name, transformTerm(tree.value, tpe)(owner))
        case Assign(lhs, rhs) =>
          Assign.copy(tree)(lhs, transformTerm(rhs, lhs.tpe.widen)(owner))
        case Block(stats, expr) =>
          Block.copy(tree)(transformStats(stats)(owner), transformTerm(expr, tpe)(owner))
        case If(cond, thenp, elsep) =>
          If.copy(tree)(
            transformTerm(cond, TypeRepr.of[Boolean])(owner),
            transformTerm(thenp, tpe)(owner),
            transformTerm(elsep, tpe)(owner))
        case _: Closure =>
          tree
        case Match(selector, cases) =>
          Match.copy(tree)(transformTerm(selector, selector.tpe)(owner), transformCaseDefs(cases, tpe)(owner))
        case Return(expr, from) =>
          // FIXME
          // ctx.owner seems to be set to the wrong symbol
          // Return.copy(tree)(transformTerm(expr, expr.tpe))
          tree
        case While(cond, body) =>
          While.copy(tree)(transformTerm(cond, TypeRepr.of[Boolean])(owner), transformTerm(body, TypeRepr.of[Any])(owner))
        case Try(block, cases, finalizer) =>
          Try.copy(tree)(transformTerm(block, tpe)(owner), transformCaseDefs(cases, TypeRepr.of[Any])(owner), finalizer.map(x => transformTerm(x, TypeRepr.of[Any])(owner)))
        case Repeated(elems, elemtpt) =>
          Repeated.copy(tree)(transformTerms(elems, elemtpt.tpe)(owner), elemtpt)
        case Inlined(call, bindings, expansion) =>
          Inlined.copy(tree)(call, transformDefinitions(bindings)(owner), transformTerm(expansion, tpe)(owner))
      }

      def transformTerm(tree: Term, tpe: TypeRepr)(owner: Symbol): Term =
        tree match
          case _: Closure =>
            tree
          case _: Inlined =>
            transformTermChildren(tree, tpe)(owner)
          case _ if tree.isExpr =>
            // WARNING: Never do a cast like this in user code (acceptable within the stdlib).
            // In theory we should use `tree.asExpr match { case '{ $expr: t } => transform(expr).asTerm }`
            // This is to avoid conflicts when re-bootstrapping the library.
            type X
            val expr = tree.asExpr.asInstanceOf[Expr[X]]
            val t = tpe.asType.asInstanceOf[Type[X]]
            val transformedExpr = transform(expr)(using t)
            transformedExpr.asTerm
          case _ =>
            transformTermChildren(tree, tpe)(owner)

      def transformTypeTree(tree: TypeTree)(owner: Symbol): TypeTree = tree

      def transformCaseDef(tree: CaseDef, tpe: TypeRepr)(owner: Symbol): CaseDef =
        CaseDef.copy(tree)(tree.pattern, tree.guard.map(x => transformTerm(x, TypeRepr.of[Boolean])(owner)), transformTerm(tree.rhs, tpe)(owner))

      def transformTypeCaseDef(tree: TypeCaseDef)(owner: Symbol): TypeCaseDef =
        TypeCaseDef.copy(tree)(transformTypeTree(tree.pattern)(owner), transformTypeTree(tree.rhs)(owner))

      def transformStats(trees: List[Statement])(owner: Symbol): List[Statement] =
        trees.mapConserve(x => transformStatement(x)(owner))

      def transformDefinitions(trees: List[Definition])(owner: Symbol): List[Definition] =
        trees.mapConserve(x => transformDefinition(x)(owner))

      def transformTerms(trees: List[Term], tpes: List[TypeRepr])(owner: Symbol): List[Term] =
        var tpes2 = tpes // TODO use proper zipConserve
        trees.mapConserve{ x =>
          val tpe :: tail = tpes2: @unchecked
          tpes2 = tail
          transformTerm(x, tpe)(owner)
        }

      def transformTerms(trees: List[Term], tpe: TypeRepr)(owner: Symbol): List[Term] =
        trees.mapConserve(x => transformTerm(x, tpe)(owner))

      def transformTypeTrees(trees: List[TypeTree])(owner: Symbol): List[TypeTree] =
        trees.mapConserve(x => transformTypeTree(x)(owner))

      def transformCaseDefs(trees: List[CaseDef], tpe: TypeRepr)(owner: Symbol): List[CaseDef] =
        trees.mapConserve(x => transformCaseDef(x, tpe)(owner))

      def transformTypeCaseDefs(trees: List[TypeCaseDef])(owner: Symbol): List[TypeCaseDef] =
        trees.mapConserve(x => transformTypeCaseDef(x)(owner))

    }
    new MapChildren()
      .transformTermChildren(e.asTerm, TypeRepr.of[T])(Symbol.spliceOwner)
      .asExprOf[T]
  }

end ExprMap
