package scala.quoted

trait ExprMap:

  /** Map an expression `e` with a type `tpe` */
  def transform[T](e: Expr[T])(using qctx: QuoteContext, tpe: Type[T]): Expr[T]

  /** Map subexpressions an expression `e` with a type `tpe` */
  def transformChildren[T](e: Expr[T])(using qctx: QuoteContext, tpe: Type[T]): Expr[T] = {
    import qctx.reflect._
    final class MapChildren() {

      def transformStatement(tree: Statement)(using ctx: Context): Statement = {
        def localCtx(definition: Definition): Context = definition.symbol.localContext
        tree match {
          case tree: Term =>
            transformTerm(tree, TypeRepr.of[Any])
          case tree: Definition =>
            transformDefinition(tree)
          case tree: Import =>
            tree
        }
      }

      def transformDefinition(tree: Definition)(using ctx: Context): Definition = {
        def localCtx(definition: Definition): Context = definition.symbol.localContext
        tree match {
          case tree: ValDef =>
            given Context = localCtx(tree)
            val rhs1 = tree.rhs.map(x => transformTerm(x, tree.tpt.tpe))
            ValDef.copy(tree)(tree.name, tree.tpt, rhs1)
          case tree: DefDef =>
            given Context = localCtx(tree)
            DefDef.copy(tree)(tree.name, tree.typeParams, tree.paramss, tree.returnTpt, tree.rhs.map(x => transformTerm(x, tree.returnTpt.tpe)))
          case tree: TypeDef =>
            tree
          case tree: ClassDef =>
            val newBody = transformStats(tree.body)
            ClassDef.copy(tree)(tree.name, tree.constructor, tree.parents, tree.derived, tree.self, newBody)
        }
      }

      def transformTermChildren(tree: Term, tpe: TypeRepr)(using ctx: Context): Term = tree match {
        case Ident(name) =>
          tree
        case Select(qualifier, name) =>
          Select.copy(tree)(transformTerm(qualifier, qualifier.tpe), name)
        case This(qual) =>
          tree
        case Super(qual, mix) =>
          tree
        case tree as Apply(fun, args) =>
          val MethodType(_, tpes, _) = fun.tpe.widen
          Apply.copy(tree)(transformTerm(fun, TypeRepr.of[Any]), transformTerms(args, tpes))
        case TypeApply(fun, args) =>
          TypeApply.copy(tree)(transformTerm(fun, TypeRepr.of[Any]), args)
        case _: Literal =>
          tree
        case New(tpt) =>
          New.copy(tree)(transformTypeTree(tpt))
        case Typed(expr, tpt) =>
          val tp = tpt.tpe match
            case AppliedType(TypeRef(ThisType(TypeRef(NoPrefix(), "scala")), "<repeated>"), List(tp0: TypeRepr)) =>
              TypeRepr.of[Seq].appliedTo(tp0)
            case tp => tp
          Typed.copy(tree)(transformTerm(expr, tp), transformTypeTree(tpt))
        case tree: NamedArg =>
          NamedArg.copy(tree)(tree.name, transformTerm(tree.value, tpe))
        case Assign(lhs, rhs) =>
          Assign.copy(tree)(lhs, transformTerm(rhs, lhs.tpe.widen))
        case Block(stats, expr) =>
          Block.copy(tree)(transformStats(stats), transformTerm(expr, tpe))
        case If(cond, thenp, elsep) =>
          If.copy(tree)(
            transformTerm(cond, TypeRepr.of[Boolean]),
            transformTerm(thenp, tpe),
            transformTerm(elsep, tpe))
        case _: Closure =>
          tree
        case Match(selector, cases) =>
          Match.copy(tree)(transformTerm(selector, selector.tpe), transformCaseDefs(cases, tpe))
        case Return(expr) =>
          // FIXME
          // ctx.owner seems to be set to the wrong symbol
          // Return.copy(tree)(transformTerm(expr, expr.tpe))
          tree
        case While(cond, body) =>
          While.copy(tree)(transformTerm(cond, TypeRepr.of[Boolean]), transformTerm(body, TypeRepr.of[Any]))
        case Try(block, cases, finalizer) =>
          Try.copy(tree)(transformTerm(block, tpe), transformCaseDefs(cases, TypeRepr.of[Any]), finalizer.map(x => transformTerm(x, TypeRepr.of[Any])))
        case Repeated(elems, elemtpt) =>
          Repeated.copy(tree)(transformTerms(elems, elemtpt.tpe), elemtpt)
        case Inlined(call, bindings, expansion) =>
          Inlined.copy(tree)(call, transformDefinitions(bindings), transformTerm(expansion, tpe)/*()call.symbol.localContext)*/)
      }

      def transformTerm(tree: Term, tpe: TypeRepr)(using ctx: Context): Term =
        tree match
          case _: Closure =>
            tree
          case _: Inlined =>
            transformTermChildren(tree, tpe)
          case _ if tree.isExpr =>
            type X
            val expr = tree.asExpr.asInstanceOf[Expr[X]]
            val t = tpe.asType.asInstanceOf[Type[X]]
            transform(expr)(using qctx, t).unseal
          case _ =>
            transformTermChildren(tree, tpe)

      def transformTypeTree(tree: TypeTree)(using ctx: Context): TypeTree = tree

      def transformCaseDef(tree: CaseDef, tpe: TypeRepr)(using ctx: Context): CaseDef =
        CaseDef.copy(tree)(tree.pattern, tree.guard.map(x => transformTerm(x, TypeRepr.of[Boolean])), transformTerm(tree.rhs, tpe))

      def transformTypeCaseDef(tree: TypeCaseDef)(using ctx: Context): TypeCaseDef = {
        TypeCaseDef.copy(tree)(transformTypeTree(tree.pattern), transformTypeTree(tree.rhs))
      }

      def transformStats(trees: List[Statement])(using ctx: Context): List[Statement] =
        trees mapConserve (transformStatement(_))

      def transformDefinitions(trees: List[Definition])(using ctx: Context): List[Definition] =
        trees mapConserve (transformDefinition(_))

      def transformTerms(trees: List[Term], tpes: List[TypeRepr])(using ctx: Context): List[Term] =
        var tpes2 = tpes // TODO use proper zipConserve
        trees mapConserve { x =>
          val tpe :: tail = tpes2
          tpes2 = tail
          transformTerm(x, tpe)
        }

      def transformTerms(trees: List[Term], tpe: TypeRepr)(using ctx: Context): List[Term] =
        trees.mapConserve(x => transformTerm(x, tpe))

      def transformTypeTrees(trees: List[TypeTree])(using ctx: Context): List[TypeTree] =
        trees mapConserve (transformTypeTree(_))

      def transformCaseDefs(trees: List[CaseDef], tpe: TypeRepr)(using ctx: Context): List[CaseDef] =
        trees mapConserve (x => transformCaseDef(x, tpe))

      def transformTypeCaseDefs(trees: List[TypeCaseDef])(using ctx: Context): List[TypeCaseDef] =
        trees mapConserve (transformTypeCaseDef(_))

    }
    new MapChildren().transformTermChildren(e.unseal, TypeRepr.of[T]).asExprOf[T]
  }

end ExprMap
