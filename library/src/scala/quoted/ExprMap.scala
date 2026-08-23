package scala.quoted

import language.experimental.captureChecking

/** A transformation on quoted expressions that maps an expression to an expression of the same type. */
trait ExprMap:

  /** Maps an expression `e` with a type `T`.
   *  Requires a given `Type[T]` instance for staging and a `Quotes` instance
   *  for access to the reflection API.
   *
   *  @tparam T the type of the expression being transformed
   *  @param e the expression to transform
   *  @return the transformed expression of type `T`
   */
  def transform[T](e: Expr[T])(using Type[T])(using Quotes): Expr[T]

  /** Maps the sub-expressions of an expression `e` with type `T`.
   *  Requires a given `Type[T]` instance for staging and a `Quotes` instance
   *  for access to the reflection API.
   *
   *  @tparam T the type of the expression whose children are transformed
   *  @param e the expression whose direct sub-expressions will be transformed via `transform`
   *  @return an expression of type `T` in which the direct sub-expressions of `e` have been
   *          replaced by the result of applying `transform` to them. Not every direct child is
   *          transformed: the left-hand side of an `Assign`, the type arguments of a `TypeApply`,
   *          the `call` of an `Inlined` tree and the expression of a `Return` are left as they
   *          are.
   *  @note If `e` is itself a bare `Closure` term, which the reflection API's `Closure.apply`
   *        can build directly rather than in the `Block`-wrapped form the compiler normally
   *        produces, it is returned completely unmodified and none of its structure, including
   *        its body, is visited.
   */
  def transformChildren[T](e: Expr[T])(using Type[T])(using Quotes): Expr[T] = {
    import quotes.reflect.*
    final class MapChildren() {

      /** Transforms a statement, delegating to `transformTerm` for terms and to
       *  `transformDefinition` for definitions.
       *
       *  @param tree the statement to transform
       *  @param owner the symbol that owns `tree`
       *  @return the transformed statement; an `Import` or `Export` is returned unchanged, as is
       *          a `Definition` that is a `TypeDef`
       */
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

      /** Transforms the right-hand side of a `ValDef` or `DefDef`, or the body of a `ClassDef`.
       *
       *  @param tree the definition to transform
       *  @param owner the symbol that owns `tree`, used when transforming the body of a
       *         `ClassDef` (the definition's own symbol owns the right-hand side of a
       *         `ValDef` or `DefDef`)
       *  @return the transformed definition; a `TypeDef` is returned unchanged
       */
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

      /** Transforms the sub-trees of a term, rebuilding `tree` from the transformed children.
       *  Some children are left as they are rather than transformed: the left-hand side of an
       *  `Assign`, the type arguments of a `TypeApply`, the `call` of an `Inlined` tree and, as
       *  described below, the expression of a `Return` and the `meth` of a `Closure`.
       *
       *  @param tree the term whose children are transformed
       *  @param tpe the expected type of `tree`, propagated to the children that share it,
       *         such as the branches of an `If` or the result expression of a `Block`
       *  @param owner the symbol that owns `tree`
       *  @return a copy of `tree` with its children transformed. `tree` itself is returned when
       *          it has no children to transform, as for an `Ident`, a `Literal`, a `This` or a
       *          `Super`, and also when transforming its children produces no change. A `Return`
       *          is likewise returned unchanged, but only as a workaround for the owner symbol
       *          being wrong at that point (see the `FIXME` in the body), so its `expr` is left
       *          untransformed even though it is a real child. A `Closure` is also returned
       *          unchanged, so its `meth` is left untransformed even though it too is a real
       *          child.
       */
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

      /** Transforms a term by applying `transform` to it when it is an expression, and by
       *  transforming its children otherwise, except for two cases: a `Closure` is returned
       *  unchanged, and an `Inlined` tree only has its children transformed, bypassing
       *  `transform` even when the `Inlined` tree is itself an expression.
       *
       *  @param tree the term to transform
       *  @param tpe the expected type of `tree`. It is used as the `Type` argument of `transform`
       *         when `tree` is an expression, is passed on to `transformTermChildren` for an
       *         `Inlined` tree and in the remaining case, and is unused for a `Closure`.
       *  @param owner the symbol that owns `tree`
       *  @return the transformed term; a `Closure` is returned unchanged and an `Inlined` tree
       *          has only its children transformed
       */
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

      /** Returns `tree` unchanged, since type trees are not transformed.
       *
       *  @param tree the type tree
       *  @param owner the symbol that owns `tree` (never used)
       */
      def transformTypeTree(tree: TypeTree)(owner: Symbol): TypeTree = tree

      /** Transforms the guard and the right-hand side of a case, leaving its pattern unchanged.
       *
       *  @param tree the case to transform
       *  @param tpe the expected type of the right-hand side of `tree`
       *  @param owner the symbol that owns `tree`
       *  @return a copy of `tree` with its guard transformed as a `Boolean` and its right-hand
       *          side transformed at type `tpe`
       */
      def transformCaseDef(tree: CaseDef, tpe: TypeRepr)(owner: Symbol): CaseDef =
        CaseDef.copy(tree)(tree.pattern, tree.guard.map(x => transformTerm(x, TypeRepr.of[Boolean])(owner)), transformTerm(tree.rhs, tpe)(owner))

      /** Returns `tree` unchanged, since `transformTypeTree` transforms neither the pattern nor
       *  the right-hand side of a type case.
       *
       *  @param tree the type case
       *  @param owner the symbol that owns `tree`, passed on to `transformTypeTree`, which
       *         ignores it, so its value has no effect
       */
      def transformTypeCaseDef(tree: TypeCaseDef)(owner: Symbol): TypeCaseDef =
        TypeCaseDef.copy(tree)(transformTypeTree(tree.pattern)(owner), transformTypeTree(tree.rhs)(owner))

      /** Transforms each statement of `trees` with `transformStatement`.
       *
       *  @param trees the statements to transform
       *  @param owner the symbol that owns the statements
       *  @return the transformed statements, or `trees` itself if no statement changed
       */
      def transformStats(trees: List[Statement])(owner: Symbol): List[Statement] =
        trees.mapConserve(x => transformStatement(x)(owner))

      /** Transforms each definition of `trees` with `transformDefinition`.
       *
       *  @param trees the definitions to transform
       *  @param owner the symbol that owns the definitions
       *  @return the transformed definitions, or `trees` itself if no definition changed
       */
      def transformDefinitions(trees: List[Definition])(owner: Symbol): List[Definition] =
        trees.mapConserve(x => transformDefinition(x)(owner))

      /** Transforms each term of `trees`, using the type at the same position in `tpes` as its
       *  expected type.
       *
       *  @param trees the terms to transform
       *  @param tpes the expected types, in the same order as `trees` and at least as many
       *  @param owner the symbol that owns the terms
       *  @return the transformed terms, or `trees` itself if no term changed
       *  @throws MatchError if `tpes` is shorter than `trees`
       */
      def transformTerms(trees: List[Term], tpes: List[TypeRepr])(owner: Symbol): List[Term] =
        var tpes2 = tpes // TODO use proper zipConserve
        trees.mapConserve{ x =>
          val tpe :: tail = tpes2: @unchecked
          tpes2 = tail
          transformTerm(x, tpe)(owner)
        }

      /** Transforms each term of `trees` with `tpe` as the expected type of every term.
       *
       *  @param trees the terms to transform
       *  @param tpe the expected type shared by all the terms
       *  @param owner the symbol that owns the terms
       *  @return the transformed terms, or `trees` itself if no term changed
       */
      def transformTerms(trees: List[Term], tpe: TypeRepr)(owner: Symbol): List[Term] =
        trees.mapConserve(x => transformTerm(x, tpe)(owner))

      /** Returns `trees` unchanged, since `transformTypeTree` does not transform type trees.
       *
       *  @param trees the type trees
       *  @param owner the symbol that owns the type trees
       */
      def transformTypeTrees(trees: List[TypeTree])(owner: Symbol): List[TypeTree] =
        trees.mapConserve(x => transformTypeTree(x)(owner))

      /** Transforms each case of `trees` with `tpe` as the expected type of every right-hand side.
       *
       *  @param trees the cases to transform
       *  @param tpe the expected type of the right-hand side of each case
       *  @param owner the symbol that owns the cases
       *  @return the transformed cases, or `trees` itself if no case changed
       */
      def transformCaseDefs(trees: List[CaseDef], tpe: TypeRepr)(owner: Symbol): List[CaseDef] =
        trees.mapConserve(x => transformCaseDef(x, tpe)(owner))

      /** Returns `trees` unchanged, since `transformTypeCaseDef` never changes a type case.
       *
       *  @param trees the type cases
       *  @param owner the symbol that owns the type cases
       */
      def transformTypeCaseDefs(trees: List[TypeCaseDef])(owner: Symbol): List[TypeCaseDef] =
        trees.mapConserve(x => transformTypeCaseDef(x)(owner))

    }
    new MapChildren()
      .transformTermChildren(e.asTerm, TypeRepr.of[T])(Symbol.spliceOwner)
      .asExprOf[T]
  }

end ExprMap
