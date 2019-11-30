package dotty.tools.tasty.experimental.bridge

import reflect.ClassTag

trait TreeOps extends Core with
  self: PositionOps with ContextOps with SourceFileOps with SymbolOps =>

  given ClassTag[Tree] = internal.Tree_CT
  given ClassTag[MemberDef] = internal.MemberDef_CT
  given ClassTag[Hole] = internal.Hole_CT
  given ClassTag[Template] = internal.Template_CT
  given ClassTag[ValOrDefDef] = internal.ValOrDefDef_CT
  given ClassTag[TypeDef] = internal.TypeDef_CT
  given ClassTag[ValDef] = internal.ValDef_CT
  given ClassTag[DefDef] = internal.DefDef_CT
  given ClassTag[Ident] = internal.Ident_CT
  given ClassTag[This] = internal.This_CT
  given ClassTag[Select] = internal.Select_CT
  given ClassTag[Apply] = internal.Apply_CT
  given ClassTag[TypeApply] = internal.TypeApply_CT
  given ClassTag[Literal] = internal.Literal_CT
  given ClassTag[Super] = internal.Super_CT
  given ClassTag[New] = internal.New_CT
  given ClassTag[Typed] = internal.Typed_CT
  given ClassTag[NamedArg] = internal.NamedArg_CT
  given ClassTag[Assign] = internal.Assign_CT
  given ClassTag[Block] = internal.Block_CT
  given ClassTag[If] = internal.If_CT
  given ClassTag[Closure] = internal.Closure_CT
  given ClassTag[Match] = internal.Match_CT
  given ClassTag[CaseDef] = internal.CaseDef_CT
  given ClassTag[Labeled] = internal.Labeled_CT
  given ClassTag[Return] = internal.Return_CT
  given ClassTag[WhileDo] = internal.WhileDo_CT
  given ClassTag[Try] = internal.Try_CT
  given ClassTag[SeqLiteral] = internal.SeqLiteral_CT
  given ClassTag[Inlined] = internal.Inlined_CT
  given ClassTag[Bind] = internal.Bind_CT
  given ClassTag[Alternative] = internal.Alternative_CT
  given ClassTag[UnApply] = internal.UnApply_CT
  given ClassTag[Import] = internal.Import_CT
  given ClassTag[PackageDef] = internal.PackageDef_CT
  given ClassTag[TypeTree] = internal.TypeTree_CT
  given ClassTag[SingletonTypeTree] = internal.SingletonTypeTree_CT
  given ClassTag[RefinedTypeTree] = internal.RefinedTypeTree_CT
  given ClassTag[AppliedTypeTree] = internal.AppliedTypeTree_CT
  given ClassTag[MatchTypeTree] = internal.MatchTypeTree_CT
  given ClassTag[ByNameTypeTree] = internal.ByNameTypeTree_CT
  given ClassTag[Annotated] = internal.Annotated_CT
  given ClassTag[LambdaTypeTree] = internal.LambdaTypeTree_CT
  given ClassTag[TypeBoundsTree] = internal.TypeBoundsTree_CT
  given ClassTag[Thicket] = internal.Thicket_CT

  given untpdTree: ClassTag[untpd.Tree] = internal.untpd_Tree_CT
  given untpdTypedSplice: ClassTag[untpd.TypedSplice] = internal.untpd_TypedSplice_CT
  given untpdMemberDef: ClassTag[untpd.MemberDef] = internal.untpd_MemberDef_CT
  given untpdIdent: ClassTag[untpd.Ident] = internal.untpd_Ident_CT

  object untpd with

    type Tree = internal.untpd_Tree
    type TypedSplice = internal.untpd_TypedSplice
    type ImportSelector = internal.untpd_ImportSelector
    type MemberDef = internal.untpd_MemberDef
    type Ident = internal.untpd_Ident

    object TypedSplice with
      def unapply(tree: TypedSplice): Some[self.Tree] = internal.untpd_TypedSplice_unapply(tree)

    object Ident with
      def unapply(tree: Ident): Some[Name] = internal.untpd_Ident_unapply(tree)

    given ImportSelectorOps: (tree: ImportSelector) with
      def imported: Ident = internal.ImportSelector_imported(tree)
      def renamed: Tree = internal.ImportSelector_renamed(tree)
      def bound: Tree = internal.ImportSelector_bound(tree)

  end untpd

  object Ident with
    def unapply(tree: Ident): Some[Name] = internal.Ident_unapply(tree)
  object This with
    def unapply(tree: This): Some[untpd.Ident] = internal.This_unapply(tree)
  object Select with
    def unapply(tree: Select): (Tree, Name) = internal.Select_unapply(tree)
  object Apply with
    def unapply(tree: Apply): (Tree, List[Tree]) = internal.Apply_unapply(tree)
  object TypeApply with
    def unapply(tree: TypeApply): (Tree, List[Tree]) = internal.TypeApply_unapply(tree)
  object Literal with
    def unapply(tree: Literal): Some[Constant] = internal.Literal_unapply(tree)
  object Super with
    def unapply(tree: Super): (Tree, untpd.Ident) = internal.Super_unapply(tree)
  object New with
    def unapply(tree: New): Some[Tree] = internal.New_unapply(tree)
  object Typed with
    def unapply(tree: Typed): (Tree, Tree) = internal.Typed_unapply(tree)
  object NamedArg with
    def unapply(tree: NamedArg): (Name, Tree) = internal.NamedArg_unapply(tree)
  object Assign with
    def unapply(tree: Assign): (Tree, Tree) = internal.Assign_unapply(tree)
  object Block with
    def unapply(tree: Block): (List[Tree], Tree) = internal.Block_unapply(tree)
  object If with
    def unapply(tree: If): (Tree, Tree, Tree) = internal.If_unapply(tree)
  object Closure with
    def unapply(tree: Closure): (List[Tree], Tree, Tree) = internal.Closure_unapply(tree)
  object Match with
    def unapply(tree: Match): (Tree, List[CaseDef]) = internal.Match_unapply(tree)
  object CaseDef with
    def unapply(tree: CaseDef): (Tree, Tree, Tree) = internal.CaseDef_unapply(tree)
  object Labeled with
    def unapply(tree: Labeled): (Bind, Tree) = internal.Labeled_unapply(tree)
  object Return with
    def unapply(tree: Return): (Tree, Tree) = internal.Return_unapply(tree)
  object WhileDo with
    def unapply(tree: WhileDo): (Tree, Tree) = internal.WhileDo_unapply(tree)
  object Try with
    def unapply(tree: Try): (Tree, List[CaseDef], Tree) = internal.Try_unapply(tree)
  object SeqLiteral with
    def unapply(tree: SeqLiteral): (List[Tree], Tree) = internal.SeqLiteral_unapply(tree)
  object Inlined with
    def unapply(tree: Inlined): (Tree, List[MemberDef], Tree) = internal.Inlined_unapply(tree)
  object Bind with
    def unapply(tree: Bind): (Name, Tree) = internal.Bind_unapply(tree)
  object Alternative with
    def unapply(tree: Alternative): Some[List[Tree]] = internal.Alternative_unapply(tree)
  object UnApply with
    def unapply(tree: UnApply): (Tree, List[Tree], List[Tree]) = internal.UnApply_unapply(tree)
  object Import with
    def unapply(tree: Import): (Tree, List[untpd.ImportSelector]) = internal.Import_unapply(tree)
  object PackageDef with
    def unapply(tree: PackageDef): (RefTree, List[Tree]) = internal.PackageDef_unapply(tree)
  object SingletonTypeTree with
    def unapply(tree: SingletonTypeTree): Some[Tree] = internal.SingletonTypeTree_unapply(tree)
  object RefinedTypeTree with
    def unapply(tree: RefinedTypeTree): (Tree, List[Tree]) = internal.RefinedTypeTree_unapply(tree)
  object AppliedTypeTree with
    def unapply(tree: AppliedTypeTree): (Tree, List[Tree]) = internal.AppliedTypeTree_unapply(tree)
  object MatchTypeTree with
    def unapply(tree: MatchTypeTree): (Tree, Tree, List[CaseDef]) = internal.MatchTypeTree_unapply(tree)
  object ByNameTypeTree with
    def unapply(tree: ByNameTypeTree): Some[Tree] = internal.ByNameTypeTree_unapply(tree)
  object Annotated with
    def unapply(tree: Annotated): (Tree, Tree) = internal.Annotated_unapply(tree)
  object LambdaTypeTree with
    def unapply(tree: LambdaTypeTree): (List[TypeDef], Tree) = internal.LambdaTypeTree_unapply(tree)
  object TypeBoundsTree with
    def unapply(tree: TypeBoundsTree): (Tree, Tree) = internal.TypeBoundsTree_unapply(tree)
  object Hole with
    def unapply(tree: Hole): (Int, List[Tree]) = internal.Hole_unapply(tree)
  object Thicket with
    def unapply(tree: Thicket): Some[List[Tree]] = internal.Thicket_unapply(tree)

  given untpdTreeOps: (tree: untpd.Tree) with
    def symbol(given Context): Symbol = internal.untpd_Tree_symbol(tree)
    def span: Span = internal.untpd_Tree_span(tree)
    def source: SourceFile = internal.untpd_Tree_source(tree)
    def envelope(src: SourceFile, startSpan: Span = Span.noSpan): Span = internal.untpd_Tree_envelope(tree, src, startSpan)
    def withType(tpe: Type)(given Context): Tree = internal.untpd_Tree_withType(tree, tpe)
    def isEmpty: Boolean = internal.untpd_Tree_isEmpty(tree)

  given TreeOps: (tree: Tree) with
    def isType: Boolean = internal.Tree_isType(tree)
    def tpe: Type = internal.Tree_tpe(tree)

  given IfOps: (tree: If) with
    def isInline: Boolean = internal.If_isInline(tree)

  given MatchOps: (tree: Match) with
    def isInline: Boolean = internal.Match_isInline(tree)

  given ValOrDefDefOps: (tree: ValOrDefDef) with
    def name: TermName = internal.ValOrDefDef_name(tree)
    def tpt: Tree = internal.ValOrDefDef_tpt(tree)
    def rhs(given Context): Tree = internal.ValOrDefDef_rhs(tree)

  given DefDefOps: (tree: DefDef) with
    def tparams: List[TypeDef] = internal.DefDef_tparams(tree)
    def vparamss: List[List[ValDef]] = internal.DefDef_vparamss(tree)

  given TypeDefOps: (tree: TypeDef) with
    def rhs: Tree = internal.TypeDef_rhs(tree)

  given TemplateOps: (tree: Template) with
    def decomposeBody(given Context): (List[Tree], List[Tree]) = internal.Template_decomposeBody(tree)
    def parents: List[Tree] = internal.Template_parents(tree)
    def self: ValDef = internal.Template_self(tree)
    def constr: DefDef = internal.Template_constr(tree)
    def body(given Context): List[Tree] = internal.Template_body(tree)
    def derived: List[untpd.Tree] = internal.Template_derived(tree)

  final val EmptyTree = internal.EmptyTree

  def inlineContext(tree: Tree)(implicit ctx: Context): Context = internal.inlineContext(tree)

  abstract class TreeAccumulator[X] { self =>
    def apply(x: X, tree: Tree)(implicit ctx: Context): X

    def apply(x: X, trees: Traversable[Tree])(implicit ctx: Context): X =
      trees.foldLeft(x)(apply)

    def foldOver(x: X, tree: Tree)(implicit ctx: Context): X =
      if (tree.source != ctx.source && tree.source.exists)
        foldOver(x, tree)(ctx.withSource(tree.source))
      else {
        def localCtx =
          if (/*tree.hasType && -- Tree is biased to tpd.Tree */ tree.symbol.exists) ctx.withOwner(tree.symbol) else ctx
        tree match {
          case Ident(name) =>
            x
          case Select(qualifier, name) =>
            this(x, qualifier)
          case This(qual) =>
            x
          case Super(qual, mix) =>
            this(x, qual)
          case Apply(fun, args) =>
            this(this(x, fun), args)
          case TypeApply(fun, args) =>
            this(this(x, fun), args)
          case Literal(const) =>
            x
          case New(tpt) =>
            this(x, tpt)
          case Typed(expr, tpt) =>
            this(this(x, expr), tpt)
          case NamedArg(name, arg) =>
            this(x, arg)
          case Assign(lhs, rhs) =>
            this(this(x, lhs), rhs)
          case Block(stats, expr) =>
            this(this(x, stats), expr)
          case If(cond, thenp, elsep) =>
            this(this(this(x, cond), thenp), elsep)
          case Closure(env, meth, tpt) =>
            this(this(this(x, env), meth), tpt)
          case Match(selector, cases) =>
            this(this(x, selector), cases)
          case CaseDef(pat, guard, body) =>
            this(this(this(x, pat), guard), body)
          case Labeled(bind, expr) =>
            this(this(x, bind), expr)
          case Return(expr, from) =>
            this(this(x, expr), from)
          case WhileDo(cond, body) =>
            this(this(x, cond), body)
          case Try(block, handler, finalizer) =>
            this(this(this(x, block), handler), finalizer)
          case SeqLiteral(elems, elemtpt) =>
            this(this(x, elems), elemtpt)
          case Inlined(call, bindings, expansion) =>
            this(this(x, bindings), expansion)(inlineContext(call))
          case _: TypeTree =>
            x
          case SingletonTypeTree(ref) =>
            this(x, ref)
          case RefinedTypeTree(tpt, refinements) =>
            this(this(x, tpt), refinements)
          case AppliedTypeTree(tpt, args) =>
            this(this(x, tpt), args)
          case LambdaTypeTree(tparams, body) =>
            implicit val ctx = localCtx
            this(this(x, tparams), body)
          case MatchTypeTree(bound, selector, cases) =>
            this(this(this(x, bound), selector), cases)
          case ByNameTypeTree(result) =>
            this(x, result)
          case TypeBoundsTree(lo, hi) =>
            this(this(x, lo), hi)
          case Bind(name, body) =>
            this(x, body)
          case Alternative(trees) =>
            this(x, trees)
          case UnApply(fun, implicits, patterns) =>
            this(this(this(x, fun), implicits), patterns)
          case tree: ValDef =>
            implicit val ctx = localCtx
            this(this(x, tree.tpt), tree.rhs)
          case tree: DefDef =>
            implicit val ctx = localCtx
            this(this(tree.vparamss.foldLeft(this(x, tree.tparams))(apply), tree.tpt), tree.rhs)
          case tree: TypeDef =>
            implicit val ctx = localCtx
            this(x, tree.rhs)
          case tree: Template if tree.derived.isEmpty =>
            this(this(this(this(x, tree.constr), tree.parents), tree.self), tree.body)
          case Import(expr, _) =>
            this(x, expr)
          case PackageDef(pid, stats) =>
            this(this(x, pid), stats)(localCtx)
          case Annotated(arg, annot) =>
            this(this(x, arg), annot)
          case Thicket(ts) =>
            this(x, ts)
          case Hole(_, args) =>
            this(x, args)
          case _ =>
            x
        }
      }
  }

  abstract class TreeTraverser extends TreeAccumulator[Unit] {
    def traverse(tree: Tree)(implicit ctx: Context): Unit
    def apply(x: Unit, tree: Tree)(implicit ctx: Context): Unit = traverse(tree)
    protected def traverseChildren(tree: Tree)(implicit ctx: Context): Unit = foldOver((), tree)
  }
