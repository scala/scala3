package scala.tasty
package reflect

trait TypeOrBoundsTreeOps extends Core {

  implicit def typeTreeAsParent(term: TypeTree): TermOrTypeTree = term.asInstanceOf[TermOrTypeTree]

  implicit class TypeOrBoundsTreeAPI(self: TypeOrBoundsTree) {
    def tpe(implicit ctx: Context): TypeOrBounds = kernel.TypeOrBoundsTree_tpe(self)
  }

  // ----- TypeTrees ------------------------------------------------

  implicit class TypeTreeAPI(self: TypeTree) {
    /** Position in the source code */
    def pos(implicit ctx: Context): Position = kernel.TypeTree_pos(self)

    /** Type of this type tree */
    def tpe(implicit ctx: Context): Type = kernel.TypeTree_tpe(self)

    /** Symbol of this type tree */
    def symbol(implicit ctx: Context): Symbol = kernel.TypeTree_symbol(self)
  }

  object IsTypeTree {
    def unapply(tpt: TypeOrBoundsTree)(implicit ctx: Context): Option[TypeTree] =
      kernel.matchTypeTree(tpt)
    def unapply(termOrTypeTree: TermOrTypeTree)(implicit ctx: Context, dummy: DummyImplicit): Option[TypeTree] =
      kernel.matchTypeTreeNotTerm(termOrTypeTree)
  }

  object TypeTree extends TypeTreeCoreModule {

    object IsInferred {
      /** Matches any Inferred and returns it */
      def unapply(tpt: TypeOrBoundsTree)(implicit ctx: Context): Option[Inferred] =
        kernel.matchTypeTree_Inferred(tpt)
    }

    /** TypeTree containing an inferred type */
    object Inferred {
      def apply(tpe: Type)(implicit ctx: Context): Inferred =
        kernel.TypeTree_Inferred_apply(tpe)
      /** Matches a TypeTree containing an inferred type */
      def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Boolean =
        kernel.matchTypeTree_Inferred(typeOrBoundsTree).isDefined
    }

    object IsIdent {
      /** Matches any Ident and returns it */
      def unapply(tpt: TypeOrBoundsTree)(implicit ctx: Context): Option[Ident] =
        kernel.matchTypeTree_Ident(tpt)
    }

    object Ident {
      // TODO def apply(name: String)(implicit ctx: Context): Ident
      def copy(original: Ident)(name: String)(implicit ctx: Context): Ident =
        kernel.TypeTree_Ident_copy(original)(name)
      def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Option[String] =
        kernel.matchTypeTree_Ident(typeOrBoundsTree).map(_.name)
    }

    object IsSelect {
      /** Matches any Select and returns it */
      def unapply(tpt: TypeOrBoundsTree)(implicit ctx: Context): Option[Select] =
        kernel.matchTypeTree_Select(tpt)
    }

    object Select {
      def apply(qualifier: Term, name: String)(implicit ctx: Context): Select =
        kernel.TypeTree_Select_apply(qualifier, name)
      def copy(original: Select)(qualifier: Term, name: String)(implicit ctx: Context): Select =
        kernel.TypeTree_Select_copy(original)(qualifier, name)
      def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Option[(Term, String)] =
        kernel.matchTypeTree_Select(typeOrBoundsTree).map(x => (x.qualifier, x.name))
    }

    object IsProjection {
      /** Matches any Projection and returns it */
      def unapply(tpt: TypeOrBoundsTree)(implicit ctx: Context): Option[Projection] =
        kernel.matchTypeTree_Projection(tpt)
    }

    object Projection {
      // TODO def apply(qualifier: TypeTree, name: String)(implicit ctx: Context): Project
      def copy(original: Projection)(qualifier: TypeTree, name: String)(implicit ctx: Context): Projection =
        kernel.TypeTree_Projection_copy(original)(qualifier, name)
      def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Option[(TypeTree, String)] =
        kernel.matchTypeTree_Projection(typeOrBoundsTree).map(x => (x.qualifier, x.name))
    }

    object IsSingleton {
      /** Matches any Singleton and returns it */
      def unapply(tpt: TypeOrBoundsTree)(implicit ctx: Context): Option[Singleton] =
        kernel.matchTypeTree_Singleton(tpt)
    }

    object Singleton {
      def apply(ref: Term)(implicit ctx: Context): Singleton =
        kernel.TypeTree_Singleton_apply(ref)
      def copy(original: Singleton)(ref: Term)(implicit ctx: Context): Singleton =
        kernel.TypeTree_Singleton_copy(original)(ref)
      def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Option[Term] =
        kernel.matchTypeTree_Singleton(typeOrBoundsTree).map(_.ref)
    }

    object IsRefined {
      /** Matches any Refined and returns it */
      def unapply(tpt: TypeOrBoundsTree)(implicit ctx: Context): Option[Refined] =
        kernel.matchTypeTree_Refined(tpt)
    }

    object Refined {
      // TODO def apply(tpt: TypeTree, refinements: List[Definition])(implicit ctx: Context): Refined
      def copy(original: Refined)(tpt: TypeTree, refinements: List[Definition])(implicit ctx: Context): Refined =
        kernel.TypeTree_Refined_copy(original)(tpt, refinements)
      def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Option[(TypeTree, List[Definition])] =
        kernel.matchTypeTree_Refined(typeOrBoundsTree).map(x => (x.tpt, x.refinements))
    }

    object IsApplied {
      /** Matches any Applied and returns it */
      def unapply(tpt: TypeOrBoundsTree)(implicit ctx: Context): Option[Applied] =
        kernel.matchTypeTree_Applied(tpt)
    }

    object Applied {
      def apply(tpt: TypeTree, args: List[TypeOrBoundsTree])(implicit ctx: Context): Applied =
        kernel.TypeTree_Applied_apply(tpt, args)
      def copy(original: Applied)(tpt: TypeTree, args: List[TypeOrBoundsTree])(implicit ctx: Context): Applied =
        kernel.TypeTree_Applied_copy(original)(tpt, args)
      def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Option[(TypeTree, List[TypeOrBoundsTree])] =
        kernel.matchTypeTree_Applied(typeOrBoundsTree).map(x => (x.tpt, x.args))
    }

    object IsAnnotated {
      /** Matches any Annotated and returns it */
      def unapply(tpt: TypeOrBoundsTree)(implicit ctx: Context): Option[Annotated] =
        kernel.matchTypeTree_Annotated(tpt)
    }

    object Annotated {
      def apply(arg: TypeTree, annotation: Term)(implicit ctx: Context): Annotated =
        kernel.TypeTree_Annotated_apply(arg, annotation)
      def copy(original: Annotated)(arg: TypeTree, annotation: Term)(implicit ctx: Context): Annotated =
        kernel.TypeTree_Annotated_copy(original)(arg, annotation)
      def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Option[(TypeTree, Term)] =
        kernel.matchTypeTree_Annotated(typeOrBoundsTree).map(x => (x.arg, x.annotation))
    }

    object IsMatchType {
      /** Matches any MatchType and returns it */
      def unapply(tpt: TypeOrBoundsTree)(implicit ctx: Context): Option[MatchType] =
        kernel.matchTypeTree_MatchType(tpt)
    }

    object MatchType {
      def apply(bound: Option[TypeTree], selector: TypeTree, cases: List[TypeCaseDef])(implicit ctx: Context): MatchType =
        kernel.TypeTree_MatchType_apply(bound, selector, cases)
      def copy(original: MatchType)(bound: Option[TypeTree], selector: TypeTree, cases: List[TypeCaseDef])(implicit ctx: Context): MatchType =
        kernel.TypeTree_MatchType_copy(original)(bound, selector, cases)
      def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Option[(Option[TypeTree], TypeTree, List[TypeCaseDef])] =
        kernel.matchTypeTree_MatchType(typeOrBoundsTree).map(x => (x.bound, x.selector, x.cases))
    }

    object IsByName {
      /** Matches any ByName and returns it */
      def unapply(tpt: TypeOrBoundsTree)(implicit ctx: Context): Option[ByName] =
        kernel.matchTypeTree_ByName(tpt)
    }

    object ByName {
      def apply(result: TypeTree)(implicit ctx: Context): ByName =
        kernel.TypeTree_ByName_apply(result)
      def copy(original: ByName)(result: TypeTree)(implicit ctx: Context): ByName =
        kernel.TypeTree_ByName_copy(original)(result)
      def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Option[TypeTree] =
        kernel.matchTypeTree_ByName(typeOrBoundsTree).map(_.result)
    }

    object IsLambdaTypeTree {
      /** Matches any LambdaTypeTree and returns it */
      def unapply(tpt: TypeOrBoundsTree)(implicit ctx: Context): Option[LambdaTypeTree] =
        kernel.matchTypeTree_LambdaTypeTree(tpt)
    }

    object LambdaTypeTree {
      def apply(tparams: List[TypeDef], body: TypeOrBoundsTree)(implicit ctx: Context): LambdaTypeTree =
        kernel.TypeTree_LambdaTypeTree_apply(tparams, body)
      def copy(original: LambdaTypeTree)(tparams: List[TypeDef], body: TypeOrBoundsTree)(implicit ctx: Context): LambdaTypeTree =
        kernel.TypeTree_LambdaTypeTree_copy(original)(tparams, body)
      def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Option[(List[TypeDef], TypeOrBoundsTree)] =
        kernel.matchTypeTree_LambdaTypeTree(typeOrBoundsTree).map(x => (x.tparams, x.body))
    }

    object IsTypeBind {
      /** Matches any TypeBind and returns it */
      def unapply(tpt: TypeOrBoundsTree)(implicit ctx: Context): Option[TypeBind] =
        kernel.matchTypeTree_TypeBind(tpt)
    }

    object TypeBind {
      // TODO def apply(name: String, tpt: TypeOrBoundsTree)(implicit ctx: Context): TypeBind
      def copy(original: TypeBind)(name: String, tpt: TypeOrBoundsTree)(implicit ctx: Context): TypeBind =
        kernel.TypeTree_TypeBind_copy(original)(name, tpt)
      def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Option[(String, TypeOrBoundsTree)] =
        kernel.matchTypeTree_TypeBind(typeOrBoundsTree).map(x => (x.name, x.body))
    }

    object IsTypeBlock {
      /** Matches any TypeBlock and returns it */
      def unapply(tpt: TypeOrBoundsTree)(implicit ctx: Context): Option[TypeBlock] =
        kernel.matchTypeTree_TypeBlock(tpt)
    }

    object TypeBlock {
      def apply(aliases: List[TypeDef], tpt: TypeTree)(implicit ctx: Context): TypeBlock =
        kernel.TypeTree_TypeBlock_apply(aliases, tpt)
      def copy(original: TypeBlock)(aliases: List[TypeDef], tpt: TypeTree)(implicit ctx: Context): TypeBlock =
        kernel.TypeTree_TypeBlock_copy(original)(aliases, tpt)
      def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Option[(List[TypeDef], TypeTree)] =
        kernel.matchTypeTree_TypeBlock(typeOrBoundsTree).map(x => (x.aliases, x.tpt))
    }
  }

  implicit class TypeTree_IdentAPI(self: TypeTree.Ident) {
    def name(implicit ctx: Context): String = kernel.TypeTree_Ident_name(self)
  }

  implicit class TypeTree_SelectAPI(self: TypeTree.Select) {
    def qualifier(implicit ctx: Context): Term = kernel.TypeTree_Select_qualifier(self)
    def name(implicit ctx: Context): String = kernel.TypeTree_Select_name(self)
  }

  implicit class TypeTree_ProjectionAPI(self: TypeTree.Projection) {
    def qualifier(implicit ctx: Context): TypeTree = kernel.TypeTree_Projection_qualifier(self)
    def name(implicit ctx: Context): String = kernel.TypeTree_Projection_name(self)
  }

  implicit class TypeTree_SingletonAPI(self: TypeTree.Singleton) {
    def ref(implicit ctx: Context): Term = kernel.TypeTree_Singleton_ref(self)
  }

  implicit class TypeTree_RefinedAPI(self: TypeTree.Refined) {
    def tpt(implicit ctx: Context): TypeTree = kernel.TypeTree_Refined_tpt(self)
    def refinements(implicit ctx: Context): List[Definition] = kernel.TypeTree_Refined_refinements(self)
  }

  implicit class TypeTree_AppliedAPI(self: TypeTree.Applied) {
    def tpt(implicit ctx: Context): TypeTree = kernel.TypeTree_Applied_tpt(self)
    def args(implicit ctx: Context): List[TypeOrBoundsTree] = kernel.TypeTree_Applied_args(self)
  }

  implicit class TypeTree_AnnotatedAPI(self: TypeTree.Annotated) {
    def arg(implicit ctx: Context): TypeTree = kernel.TypeTree_Annotated_arg(self)
    def annotation(implicit ctx: Context): Term = kernel.TypeTree_Annotated_annotation(self)
  }

  implicit class TypeTree_MatchTypeAPI(self: TypeTree.MatchType) {
    def bound(implicit ctx: Context): Option[TypeTree] = kernel.TypeTree_MatchType_bound(self)
    def selector(implicit ctx: Context): TypeTree = kernel.TypeTree_MatchType_selector(self)
    def cases(implicit ctx: Context): List[TypeCaseDef] = kernel.TypeTree_MatchType_cases(self)
  }

  implicit class TypeTree_ByNameAPI(self: TypeTree.ByName) {
    def result(implicit ctx: Context): TypeTree = kernel.TypeTree_ByName_result(self)
  }

  implicit class TypeTree_LambdaTypeTreeAPI(self: TypeTree.LambdaTypeTree) {
    def tparams(implicit ctx: Context): List[TypeDef] = kernel.TypeTree_LambdaTypeTree_tparams(self)
    def body(implicit ctx: Context): TypeOrBoundsTree = kernel.TypeTree_LambdaTypeTree_body(self)
  }

  implicit class TypeTree_TypeBindAPI(self: TypeTree.TypeBind) {
    def name(implicit ctx: Context): String = kernel.TypeTree_TypeBind_name(self)
    def body(implicit ctx: Context): TypeOrBoundsTree = kernel.TypeTree_TypeBind_body(self)
  }

  implicit class TypeTree_TypeBlockAPI(self: TypeTree.TypeBlock) {
    def aliases(implicit ctx: Context): List[TypeDef] = kernel.TypeTree_TypeBlock_aliases(self)
    def tpt(implicit ctx: Context): TypeTree = kernel.TypeTree_TypeBlock_tpt(self)
  }

  // ----- TypeBoundsTrees ------------------------------------------------

  implicit class TypeBoundsTreeAPI(self: TypeBoundsTree) {
    def tpe(implicit ctx: Context): TypeBounds = kernel.TypeBoundsTree_tpe(self)
    def low(implicit ctx: Context): TypeTree = kernel.TypeBoundsTree_low(self)
    def hi(implicit ctx: Context): TypeTree = kernel.TypeBoundsTree_hi(self)
  }

  object IsTypeBoundsTree {
    def unapply(tpt: TypeOrBoundsTree)(implicit ctx: Context): Option[TypeBoundsTree] =
      kernel.matchTypeBoundsTree(tpt)
  }

  object TypeBoundsTree {
    def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Option[(TypeTree, TypeTree)] =
      kernel.matchTypeBoundsTree(typeOrBoundsTree).map(x => (x.low, x.hi))
  }

  object IsWildcardTypeTree {
    def unapply(tpt: TypeOrBoundsTree)(implicit ctx: Context): Option[WildcardTypeTree] =
      kernel.matchWildcardTypeTree(tpt)
  }

  /** TypeBoundsTree containing wildcard type bounds */
  object WildcardTypeTree {
    /** Matches a TypeBoundsTree containing wildcard type bounds */
    def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Boolean =
      kernel.matchWildcardTypeTree(typeOrBoundsTree).isDefined
  }

}
