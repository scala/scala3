package scala.quoted.util

import scala.quoted._

trait ExprMap {

  /** Map an expression `e` with a type `tpe` */
  def transform[T](e: Expr[T])(given qctx: QuoteContext, tpe: Type[T]): Expr[T]

  /** Map subexpressions an expression `e` with a type `tpe` */
  def transformChildren[T](e: Expr[T])(given qctx: QuoteContext, tpe: Type[T]): Expr[T] = {
    import qctx.tasty.{_, given}
    class MapChildren() {

      def transformStatement(tree: Statement)(given ctx: Context): Statement = {
        def localCtx(definition: Definition): Context = definition.symbol.localContext
        tree match {
          case tree: Term =>
            transformTerm(tree, defn.AnyType)
          case tree: Definition =>
            transformDefinition(tree)
          case tree: Import =>
            tree
        }
      }

      def transformDefinition(tree: Definition)(given ctx: Context): Definition = {
        def localCtx(definition: Definition): Context = definition.symbol.localContext
        tree match {
          case tree: ValDef =>
            implicit val ctx = localCtx(tree)
            val rhs1 = tree.rhs.map(x => transformTerm(x, tree.tpt.tpe))
            ValDef.copy(tree)(tree.name, tree.tpt, rhs1)
          case tree: DefDef =>
            implicit val ctx = localCtx(tree)
            DefDef.copy(tree)(tree.name, tree.typeParams, tree.paramss, tree.returnTpt, tree.rhs.map(x => transformTerm(x, tree.returnTpt.tpe)))
          case tree: TypeDef =>
            tree
          case tree: ClassDef =>
            val newBody = transformStats(tree.body)
            ClassDef.copy(tree)(tree.name, tree.constructor, tree.parents, tree.derived, tree.self, newBody)
        }
      }

      def transformTermChildren(tree: Term, tpe: Type)(given ctx: Context): Term = tree match {
        case Ident(name) =>
          tree
        case Select(qualifier, name) =>
          Select.copy(tree)(transformTerm(qualifier, qualifier.tpe.widen), name)
        case This(qual) =>
          tree
        case Super(qual, mix) =>
          tree
        case tree @ Apply(fun, args) =>
          val MethodType(_, tpes, _) = fun.tpe.widen
          Apply.copy(tree)(transformTerm(fun, defn.AnyType), transformTerms(args, tpes))
        case TypeApply(fun, args) =>
          TypeApply.copy(tree)(transformTerm(fun, defn.AnyType), args)
        case _: Literal =>
          tree
        case New(tpt) =>
          New.copy(tree)(transformTypeTree(tpt))
        case Typed(expr, tpt) =>
          val tp = tpt.tpe match
            // TODO improve code
            case AppliedType(TypeRef(ThisType(TypeRef(NoPrefix(), "scala")), "<repeated>"), List(tp0: Type)) =>
              type T
              val a = tp0.seal.asInstanceOf[quoted.Type[T]]
              '[Seq[$a]].unseal.tpe
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
            transformTerm(cond, defn.BooleanType),
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
          While.copy(tree)(transformTerm(cond, defn.BooleanType), transformTerm(body, defn.AnyType))
        case Try(block, cases, finalizer) =>
          Try.copy(tree)(transformTerm(block, tpe), transformCaseDefs(cases, defn.AnyType), finalizer.map(x => transformTerm(x, defn.AnyType)))
        case Repeated(elems, elemtpt) =>
          Repeated.copy(tree)(transformTerms(elems, elemtpt.tpe), elemtpt)
        case Inlined(call, bindings, expansion) =>
          Inlined.copy(tree)(call, transformDefinitions(bindings), transformTerm(expansion, tpe)/*()call.symbol.localContext)*/)
      }

      def transformTerm(tree: Term, tpe: Type)(given ctx: Context): Term =
        tree match {
        case _: Closure =>
          tree
        case _: Inlined =>
          transformTermChildren(tree, tpe)
        case _ =>
          tree.tpe.widen match {
            case _: MethodType | _: PolyType =>
              transformTermChildren(tree, tpe)
            case _ =>
              type X
              val expr = tree.seal.asInstanceOf[Expr[X]]
              val t = tpe.seal.asInstanceOf[quoted.Type[X]]
              transform(expr)(given qctx, t).unseal
          }
      }

      def transformTypeTree(tree: TypeTree)(given ctx: Context): TypeTree = tree

      def transformCaseDef(tree: CaseDef, tpe: Type)(given ctx: Context): CaseDef =
        CaseDef.copy(tree)(tree.pattern, tree.guard.map(x => transformTerm(x, defn.BooleanType)), transformTerm(tree.rhs, tpe))

      def transformTypeCaseDef(tree: TypeCaseDef)(given ctx: Context): TypeCaseDef = {
        TypeCaseDef.copy(tree)(transformTypeTree(tree.pattern), transformTypeTree(tree.rhs))
      }

      def transformStats(trees: List[Statement])(given ctx: Context): List[Statement] =
        trees mapConserve (transformStatement(_))

      def transformDefinitions(trees: List[Definition])(given ctx: Context): List[Definition] =
        trees mapConserve (transformDefinition(_))

      def transformTerms(trees: List[Term], tpes: List[Type])(given ctx: Context): List[Term] =
        var tpes2 = tpes // TODO use proper zipConserve
        trees mapConserve { x =>
          val tpe :: tail = tpes2
          tpes2 = tail
          transformTerm(x, tpe)
        }

      def transformTerms(trees: List[Term], tpe: Type)(given ctx: Context): List[Term] =
        trees.mapConserve(x => transformTerm(x, tpe))

      def transformTypeTrees(trees: List[TypeTree])(given ctx: Context): List[TypeTree] =
        trees mapConserve (transformTypeTree(_))

      def transformCaseDefs(trees: List[CaseDef], tpe: Type)(given ctx: Context): List[CaseDef] =
        trees mapConserve (x => transformCaseDef(x, tpe))

      def transformTypeCaseDefs(trees: List[TypeCaseDef])(given ctx: Context): List[TypeCaseDef] =
        trees mapConserve (transformTypeCaseDef(_))

    }
    new MapChildren().transformTermChildren(e.unseal, tpe.unseal.tpe).seal.cast[T] // Cast will only fail if this implementation has a bug
  }

}
