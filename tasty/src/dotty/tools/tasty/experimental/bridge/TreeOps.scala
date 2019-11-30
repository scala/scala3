package dotty.tools.tasty.experimental.bridge

import reflect.ClassTag

trait TreeOps extends Core with
  self =>

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

  object untpd with

    type Tree = internal.untpd_Tree
    type TypedSplice = internal.untpd_TypedSplice
    type ImportSelector = internal.untpd_ImportSelector

    given ClassTag[Tree] = internal.untpd_Tree_CT
    given ClassTag[TypedSplice] = internal.untpd_TypedSplice_CT

    object TypedSplice with
      def unapply(tree: TypedSplice): Some[self.Tree] = internal.TypedSplice_unapply(tree)

    given ImportSelectorOps: (tree: ImportSelector) with
      def imported: Ident = internal.ImportSelector_imported(tree)
      def renamed: self.Tree = internal.ImportSelector_renamed(tree)
      def bound: Tree = internal.ImportSelector_bound(tree)

  end untpd

  object Ident with
    def unapply(tree: Ident): Some[Name] = internal.Ident_unapply(tree)
  object This with
    def unapply(tree: This): Some[Ident] = internal.This_unapply(tree)
  object Select with
    def unapply(tree: Select): (Tree, Name) = internal.Select_unapply(tree)
  object Apply with
    def unapply(tree: Apply): (Tree, List[Tree]) = internal.Apply_unapply(tree)
  object TypeApply with
    def unapply(tree: TypeApply): (Tree, List[Tree]) = internal.TypeApply_unapply(tree)
  object Literal with
    def unapply(tree: Literal): Some[Constant] = internal.Literal_unapply(tree)
  object Super with
    def unapply(tree: Super): (Tree, Ident) = internal.Super_unapply(tree)
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

  given TreeOps: (tree: Tree) with
    def symbol(given Context): Symbol = internal.Tree_symbol(tree)
    def isEmpty: Boolean = internal.Tree_isEmpty(tree)
    def isType: Boolean = internal.Tree_isType(tree)
    def withType(tpe: Type)(given Context): tree.ThisTree = internal.Tree_withType(tree, tpe)
    def isInline: Boolean = internal.Tree_isInline(tree)
    def tpe: Type = internal.Tree_tpe(tree)

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

  final val EmptyTree = internal.EmptyTree
