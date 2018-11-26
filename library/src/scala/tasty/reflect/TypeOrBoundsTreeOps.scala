package scala.tasty
package reflect

trait TypeOrBoundsTreeOps extends Core {

  trait TypeOrBoundsTreeAPI {
    def tpe(implicit ctx: Context): TypeOrBounds
  }
  implicit def TypeOrBoundsTreeDeco(tpt: TypeOrBoundsTree): TypeOrBoundsTreeAPI

  // ----- TypeTrees ------------------------------------------------

  trait TypeTreeAPI {
    /** Position in the source code */
    def pos(implicit ctx: Context): Position

    def tpe(implicit ctx: Context): Type
    def symbol(implicit ctx: Context): Symbol
  }
  implicit def TypeTreeDeco(tpt: TypeTree): TypeTreeAPI

  val IsTypeTree: IsTypeTreeExtractor
  abstract class IsTypeTreeExtractor {
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
    implicit def InferredDeco(x: Inferred): InferredAPI

    /** TypeTree containing an inferred type */
    val Inferred: InferredExtractor
    abstract class InferredExtractor {
      /** Matches a TypeTree containing an inferred type */
      def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Boolean
    }

    val IsIdent: IsIdentModule
    abstract class IsIdentModule {
      /** Matches any Ident and returns it */
      def unapply(tpt: TypeOrBoundsTree)(implicit ctx: Context): Option[Ident]
    }

    trait IdentAPI {
    }
    implicit def IdentDeco(x: Ident): IdentAPI

    val Ident: IdentExtractor
    abstract class IdentExtractor {
      def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Option[String]
    }

    val IsSelect: IsSelectModule
    abstract class IsSelectModule {
      /** Matches any Select and returns it */
      def unapply(tpt: TypeOrBoundsTree)(implicit ctx: Context): Option[Select]
    }

    trait SelectAPI {
    }
    implicit def SelectDeco(x: Select): SelectAPI

    val Select: SelectExtractor
    abstract class SelectExtractor {
      def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Option[(Term, String)]
    }

    val IsProject: IsProjectModule
    abstract class IsProjectModule {
      /** Matches any Project and returns it */
      def unapply(tpt: TypeOrBoundsTree)(implicit ctx: Context): Option[Project]
    }

    trait ProjectAPI {
    }
    implicit def ProjectDeco(x: Project): ProjectAPI

    val Project: ProjectExtractor
    abstract class ProjectExtractor {
      def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Option[(TypeTree, String)]
    }

    val IsSingleton: IsSingletonModule
    abstract class IsSingletonModule {
      /** Matches any Singleton and returns it */
      def unapply(tpt: TypeOrBoundsTree)(implicit ctx: Context): Option[Singleton]
    }

    trait SingletonAPI {
    }
    implicit def SingletonDeco(x: Singleton): SingletonAPI

    val Singleton: SingletonExtractor
    abstract class SingletonExtractor {
      def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Option[Term]
    }

    val IsRefined: IsRefinedModule
    abstract class IsRefinedModule {
      /** Matches any Refined and returns it */
      def unapply(tpt: TypeOrBoundsTree)(implicit ctx: Context): Option[Refined]
    }

    trait RefinedAPI {
    }
    implicit def RefinedDeco(x: Refined): RefinedAPI

    val Refined: RefinedExtractor
    abstract class RefinedExtractor {
      def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Option[(TypeTree, List[Definition])]
    }

    val IsApplied: IsAppliedModule
    abstract class IsAppliedModule {
      /** Matches any Applied and returns it */
      def unapply(tpt: TypeOrBoundsTree)(implicit ctx: Context): Option[Applied]
    }

    trait AppliedAPI {
    }
    implicit def AppliedDeco(x: Applied): AppliedAPI

    val Applied: AppliedExtractor
    abstract class AppliedExtractor {
      def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Option[(TypeTree, List[TypeOrBoundsTree])]
    }

    val IsAnnotated: IsAnnotatedModule
    abstract class IsAnnotatedModule {
      /** Matches any Annotated and returns it */
      def unapply(tpt: TypeOrBoundsTree)(implicit ctx: Context): Option[Annotated]
    }

    trait AnnotatedAPI {
    }
    implicit def AnnotatedDeco(x: Annotated): AnnotatedAPI

    val Annotated: AnnotatedExtractor
    abstract class AnnotatedExtractor {
      def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Option[(TypeTree, Term)]
    }

    val IsAnd: IsAndModule
    abstract class IsAndModule {
      /** Matches any And and returns it */
      def unapply(tpt: TypeOrBoundsTree)(implicit ctx: Context): Option[And]
    }

    trait AndAPI {
    }
    implicit def AndDeco(x: And): OrAPI

    val And: AndExtractor
    abstract class AndExtractor {
      def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Option[(TypeTree, TypeTree)]
    }

    val IsOr: IsOrModule
    abstract class IsOrModule {
      /** Matches any Or and returns it */
      def unapply(tpt: TypeOrBoundsTree)(implicit ctx: Context): Option[Or]
    }

    trait OrAPI {
    }
    implicit def OrDeco(x: Or): OrAPI

    val Or: OrExtractor
    abstract class OrExtractor {
      def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Option[(TypeTree, TypeTree)]
    }

    val IsMatchType: IsMatchTypeModule
    abstract class IsMatchTypeModule {
      /** Matches any MatchType and returns it */
      def unapply(tpt: TypeOrBoundsTree)(implicit ctx: Context): Option[MatchType]
    }

    trait MatchTypeAPI {
    }
    implicit def MatchTypeDeco(x: MatchType): MatchTypeAPI

    val MatchType: MatchTypeExtractor
    abstract class MatchTypeExtractor {
      def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Option[(Option[TypeTree], TypeTree, List[TypeCaseDef])]
    }

    val IsByName: IsByNameModule
    abstract class IsByNameModule {
      /** Matches any ByName and returns it */
      def unapply(tpt: TypeOrBoundsTree)(implicit ctx: Context): Option[ByName]
    }

    trait ByNameAPI {
    }
    implicit def ByNameDeco(x: ByName): ByNameAPI

    val ByName: ByNameExtractor
    abstract class ByNameExtractor {
      def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Option[TypeTree]
    }

    val IsLambdaTypeTree: IsLambdaTypeTreeModule
    abstract class IsLambdaTypeTreeModule {
      /** Matches any LambdaTypeTree and returns it */
      def unapply(tpt: TypeOrBoundsTree)(implicit ctx: Context): Option[LambdaTypeTree]
    }

    trait LambdaTypeTreeAPI {
    }
    implicit def LambdaTypeTreeDeco(x: LambdaTypeTree): LambdaTypeTreeAPI

    val LambdaTypeTree: LambdaTypeTreeExtractor
    abstract class LambdaTypeTreeExtractor {
      def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Option[(List[TypeDef], TypeOrBoundsTree)]
    }

    val IsBind: IsBindModule
    abstract class IsBindModule {
      /** Matches any Bind and returns it */
      def unapply(tpt: TypeOrBoundsTree)(implicit ctx: Context): Option[Bind]
    }

    trait BindAPI {
    }
    implicit def BindDeco(x: Bind): BindAPI

    val Bind: BindExtractor
    abstract class BindExtractor {
      def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Option[(String, TypeOrBoundsTree)]
    }

    val IsBlock: IsBlockModule
    abstract class IsBlockModule {
      /** Matches any Block and returns it */
      def unapply(tpt: TypeOrBoundsTree)(implicit ctx: Context): Option[Block]
    }

    trait BlockAPI {
    }
    implicit def BlockDeco(x: Block): BlockAPI

    val Block: BlockExtractor
    abstract class BlockExtractor {
      def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Option[(List[TypeDef], TypeTree)]
    }
  }

  // ----- TypeBoundsTrees ------------------------------------------------

  trait TypeBoundsTreeAPI {
    def tpe(implicit ctx: Context): TypeBounds
    def low(implicit ctx: Context): TypeTree
    def hi(implicit ctx: Context): TypeTree
  }
  implicit def TypeBoundsTreeDeco(tpt: TypeBoundsTree): TypeBoundsTreeAPI

  val IsTypeBoundsTree: IsTypeBoundsTreeExtractor
  abstract class IsTypeBoundsTreeExtractor {
    def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Option[TypeBoundsTree]
  }

  val TypeBoundsTree: TypeBoundsTreeExtractor
  abstract class TypeBoundsTreeExtractor {
    def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Option[(TypeTree, TypeTree)]
  }

  /** TypeBoundsTree containing wildcard type bounds */
  val WildcardTypeTree: WildcardTypeTreeExtractor
  abstract class WildcardTypeTreeExtractor {
    /** Matches a TypeBoundsTree containing wildcard type bounds */
    def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Boolean
  }

  implicit def typeTreeAsParent(term: TypeTree): TermOrTypeTree
}
