package scala.tasty
package reflect

trait TypeOrBoundsTreeOps extends Core {

  implicit def typeTreeAsParent(term: TypeTree): TermOrTypeTree

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

  val IsTypeTree: IsTypeTreeModule
  abstract class IsTypeTreeModule {
    def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Option[TypeTree]
    def unapply(parent: TermOrTypeTree)(implicit ctx: Context, dummy: DummyImplicit): Option[TypeTree]
  }

  val TypeTree: TypeTreeModule
  abstract class TypeTreeModule extends TypeTreeCoreModule {

    val IsInferred: IsInferredModule
    abstract class IsInferredModule {
      /** Matches any Inferred and returns it */
      def unapply(tpt: TypeOrBoundsTree)(implicit ctx: Context): Option[Inferred]
    }

    /** TypeTree containing an inferred type */
    val Inferred: InferredModule
    abstract class InferredModule {
      def apply(tpe: Type)(implicit ctx: Context): Inferred
      /** Matches a TypeTree containing an inferred type */
      def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Boolean
    }

    val IsIdent: IsIdentModule
    abstract class IsIdentModule {
      /** Matches any Ident and returns it */
      def unapply(tpt: TypeOrBoundsTree)(implicit ctx: Context): Option[Ident]
    }

    val Ident: IdentModule
    abstract class IdentModule {
      // TODO def apply(name: String)(implicit ctx: Context): Ident
      def copy(original: Ident)(name: String)(implicit ctx: Context): Ident
      def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Option[String]
    }

    val IsSelect: IsSelectModule
    abstract class IsSelectModule {
      /** Matches any Select and returns it */
      def unapply(tpt: TypeOrBoundsTree)(implicit ctx: Context): Option[Select]
    }

    val Select: SelectModule
    abstract class SelectModule {
      def apply(qualifier: Term, name: String)(implicit ctx: Context): Select
      def copy(original: Select)(qualifier: Term, name: String)(implicit ctx: Context): Select
      def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Option[(Term, String)]
    }

    val IsProjection: IsProjectionModule
    abstract class IsProjectionModule {
      /** Matches any Projection and returns it */
      def unapply(tpt: TypeOrBoundsTree)(implicit ctx: Context): Option[Projection]
    }

    val Projection: ProjectionModule
    abstract class ProjectionModule {
      // TODO def apply(qualifier: TypeTree, name: String)(implicit ctx: Context): Project
      def copy(original: Projection)(qualifier: TypeTree, name: String)(implicit ctx: Context): Projection
      def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Option[(TypeTree, String)]
    }

    val IsSingleton: IsSingletonModule
    abstract class IsSingletonModule {
      /** Matches any Singleton and returns it */
      def unapply(tpt: TypeOrBoundsTree)(implicit ctx: Context): Option[Singleton]
    }

    val Singleton: SingletonModule
    abstract class SingletonModule {
      def apply(ref: Term)(implicit ctx: Context): Singleton
      def copy(original: Singleton)(ref: Term)(implicit ctx: Context): Singleton
      def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Option[Term]
    }

    val IsRefined: IsRefinedModule
    abstract class IsRefinedModule {
      /** Matches any Refined and returns it */
      def unapply(tpt: TypeOrBoundsTree)(implicit ctx: Context): Option[Refined]
    }

    val Refined: RefinedModule
    abstract class RefinedModule {
      // TODO def apply(tpt: TypeTree, refinements: List[Definition])(implicit ctx: Context): Refined
      def copy(original: Refined)(tpt: TypeTree, refinements: List[Definition])(implicit ctx: Context): Refined
      def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Option[(TypeTree, List[Definition])]
    }

    val IsApplied: IsAppliedModule
    abstract class IsAppliedModule {
      /** Matches any Applied and returns it */
      def unapply(tpt: TypeOrBoundsTree)(implicit ctx: Context): Option[Applied]
    }

    val Applied: AppliedModule
    abstract class AppliedModule {
      def apply(tpt: TypeTree, args: List[TypeOrBoundsTree])(implicit ctx: Context): Applied
      def copy(original: Applied)(tpt: TypeTree, args: List[TypeOrBoundsTree])(implicit ctx: Context): Applied
      def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Option[(TypeTree, List[TypeOrBoundsTree])]
    }

    val IsAnnotated: IsAnnotatedModule
    abstract class IsAnnotatedModule {
      /** Matches any Annotated and returns it */
      def unapply(tpt: TypeOrBoundsTree)(implicit ctx: Context): Option[Annotated]
    }

    val Annotated: AnnotatedModule
    abstract class AnnotatedModule {
      def apply(arg: TypeTree, annotation: Term)(implicit ctx: Context): Annotated
      def copy(original: Annotated)(arg: TypeTree, annotation: Term)(implicit ctx: Context): Annotated
      def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Option[(TypeTree, Term)]
    }

    val IsMatchType: IsMatchTypeModule
    abstract class IsMatchTypeModule {
      /** Matches any MatchType and returns it */
      def unapply(tpt: TypeOrBoundsTree)(implicit ctx: Context): Option[MatchType]
    }

    val MatchType: MatchTypeModule
    abstract class MatchTypeModule {
      def apply(bound: Option[TypeTree], selector: TypeTree, cases: List[TypeCaseDef])(implicit ctx: Context): MatchType
      def copy(original: MatchType)(bound: Option[TypeTree], selector: TypeTree, cases: List[TypeCaseDef])(implicit ctx: Context): MatchType
      def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Option[(Option[TypeTree], TypeTree, List[TypeCaseDef])]
    }

    val IsByName: IsByNameModule
    abstract class IsByNameModule {
      /** Matches any ByName and returns it */
      def unapply(tpt: TypeOrBoundsTree)(implicit ctx: Context): Option[ByName]
    }

    val ByName: ByNameModule
    abstract class ByNameModule {
      def apply(result: TypeTree)(implicit ctx: Context): ByName
      def copy(original: ByName)(result: TypeTree)(implicit ctx: Context): ByName
      def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Option[TypeTree]
    }

    val IsLambdaTypeTree: IsLambdaTypeTreeModule
    abstract class IsLambdaTypeTreeModule {
      /** Matches any LambdaTypeTree and returns it */
      def unapply(tpt: TypeOrBoundsTree)(implicit ctx: Context): Option[LambdaTypeTree]
    }

    val LambdaTypeTree: LambdaTypeTreeModule
    abstract class LambdaTypeTreeModule {
      def apply(tparams: List[TypeDef], body: TypeOrBoundsTree)(implicit ctx: Context): LambdaTypeTree
      def copy(original: LambdaTypeTree)(tparams: List[TypeDef], body: TypeOrBoundsTree)(implicit ctx: Context): LambdaTypeTree
      def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Option[(List[TypeDef], TypeOrBoundsTree)]
    }

    val IsTypeBind: IsTypeBindModule
    abstract class IsTypeBindModule {
      /** Matches any TypeBind and returns it */
      def unapply(tpt: TypeOrBoundsTree)(implicit ctx: Context): Option[TypeBind]
    }

    val TypeBind: TypeBindModule
    abstract class TypeBindModule {
      // TODO def apply(name: String, tpt: TypeOrBoundsTree)(implicit ctx: Context): TypeBind
      def copy(original: TypeBind)(name: String, tpt: TypeOrBoundsTree)(implicit ctx: Context): TypeBind
      def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Option[(String, TypeOrBoundsTree)]
    }

    val IsTypeBlock: IsTypeBlockModule
    abstract class IsTypeBlockModule {
      /** Matches any TypeBlock and returns it */
      def unapply(tpt: TypeOrBoundsTree)(implicit ctx: Context): Option[TypeBlock]
    }

    val TypeBlock: TypeBlockModule
    abstract class TypeBlockModule {
      def apply(aliases: List[TypeDef], tpt: TypeTree)(implicit ctx: Context): TypeBlock
      def copy(original: TypeBlock)(aliases: List[TypeDef], tpt: TypeTree)(implicit ctx: Context): TypeBlock
      def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Option[(List[TypeDef], TypeTree)]
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

  val IsTypeBoundsTree: IsTypeBoundsTreeModule
  abstract class IsTypeBoundsTreeModule {
    def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Option[TypeBoundsTree]
  }

  val TypeBoundsTree: TypeBoundsTreeModule
  abstract class TypeBoundsTreeModule {
    def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Option[(TypeTree, TypeTree)]
  }

  /** TypeBoundsTree containing wildcard type bounds */
  val WildcardTypeTree: WildcardTypeTreeModule
  abstract class WildcardTypeTreeModule {
    /** Matches a TypeBoundsTree containing wildcard type bounds */
    def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Boolean
  }

}
