package scala.tasty
package reflect

trait TypeOrBoundsTreeOps extends Core {

  implicit def TypeOrBoundsTreeDeco(tpt: TypeOrBoundsTree): TypeOrBoundsTreeAPI

  implicit def TypeTreeDeco(tpt: TypeTree): TypeTreeAPI
  implicit def InferredDeco(x: TypeTree.Inferred): TypeTree.InferredAPI
  implicit def TypeIdentDeco(x: TypeTree.Ident): TypeTree.IdentAPI
  implicit def TypeSelectDeco(x: TypeTree.Select): TypeTree.SelectAPI
  implicit def ProjectionDeco(x: TypeTree.Projection): TypeTree.ProjectionAPI
  implicit def SingletonDeco(x: TypeTree.Singleton): TypeTree.SingletonAPI
  implicit def RefinedDeco(x: TypeTree.Refined): TypeTree.RefinedAPI
  implicit def AppliedDeco(x: TypeTree.Applied): TypeTree.AppliedAPI
  implicit def AnnotatedDeco(x: TypeTree.Annotated): TypeTree.AnnotatedAPI
  implicit def MatchTypeTreeDeco(x: TypeTree.MatchType): TypeTree.MatchTypeAPI
  implicit def ByNameDeco(x: TypeTree.ByName): TypeTree.ByNameAPI
  implicit def LambdaTypeTreeDeco(x: TypeTree.LambdaTypeTree): TypeTree.LambdaTypeTreeAPI
  implicit def TypeBindDeco(x: TypeTree.TypeBind): TypeTree.TypeBindAPI
  implicit def TypeBlockDeco(x: TypeTree.TypeBlock): TypeTree.TypeBlockAPI

  implicit def TypeBoundsTreeDeco(tpt: TypeBoundsTree): TypeBoundsTreeAPI

  implicit def typeTreeAsParent(term: TypeTree): TermOrTypeTree

  trait TypeOrBoundsTreeAPI {
    def tpe(implicit ctx: Context): TypeOrBounds
  }

  // ----- TypeTrees ------------------------------------------------

  trait TypeTreeAPI {
    /** Position in the source code */
    def pos(implicit ctx: Context): Position

    def tpe(implicit ctx: Context): Type
    def symbol(implicit ctx: Context): Symbol
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

    trait InferredAPI {
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

    trait IdentAPI {
      def name(implicit ctx: Context): String
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

    trait SelectAPI {
      def qualifier(implicit ctx: Context): Term
      def name(implicit ctx: Context): String
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

    trait ProjectionAPI {
      def qualifier(implicit ctx: Context): TypeTree
      def name(implicit ctx: Context): String
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

    trait SingletonAPI {
      def ref(implicit ctx: Context): Term
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

    trait RefinedAPI {
      def tpt(implicit ctx: Context): TypeTree
      def refinements(implicit ctx: Context): List[Definition]
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

    trait AppliedAPI {
      def tpt(implicit ctx: Context): TypeTree
      def args(implicit ctx: Context): List[TypeOrBoundsTree]
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

    trait AnnotatedAPI {
      def arg(implicit ctx: Context): TypeTree
      def annotation(implicit ctx: Context): Term
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

    trait MatchTypeAPI {
      def bound(implicit ctx: Context): Option[TypeTree]
      def selector(implicit ctx: Context): TypeTree
      def cases(implicit ctx: Context): List[TypeCaseDef]
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

    trait ByNameAPI {
      def result(implicit ctx: Context): TypeTree
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

    trait LambdaTypeTreeAPI {
      def tparams(implicit ctx: Context): List[TypeDef]
      def body(implicit ctx: Context): TypeOrBoundsTree
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

    trait TypeBindAPI {
      def name(implicit ctx: Context): String
      def body(implicit ctx: Context): TypeOrBoundsTree
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

    trait TypeBlockAPI {
      def aliases(implicit ctx: Context): List[TypeDef]
      def tpt(implicit ctx: Context): TypeTree
    }

    val TypeBlock: TypeBlockModule
    abstract class TypeBlockModule {
      def apply(aliases: List[TypeDef], tpt: TypeTree)(implicit ctx: Context): TypeBlock
      def copy(original: TypeBlock)(aliases: List[TypeDef], tpt: TypeTree)(implicit ctx: Context): TypeBlock
      def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Option[(List[TypeDef], TypeTree)]
    }
  }

  // ----- TypeBoundsTrees ------------------------------------------------

  trait TypeBoundsTreeAPI {
    def tpe(implicit ctx: Context): TypeBounds
    def low(implicit ctx: Context): TypeTree
    def hi(implicit ctx: Context): TypeTree
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
