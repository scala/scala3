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
  abstract class TypeTreeModule {

    /** TypeTree containing an inferred type */
    val Synthetic: SyntheticExtractor
    abstract class SyntheticExtractor {
      /** Matches a TypeTree containing an inferred type */
      def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Boolean
    }

    val Ident: IdentExtractor
    abstract class IdentExtractor {
      def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Option[String]
    }

    val Select: SelectExtractor
    abstract class SelectExtractor {
      def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Option[(Term, String)]
    }

    val Project: ProjectExtractor
    abstract class ProjectExtractor {
      def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Option[(TypeTree, String)]
    }

    val Singleton: SingletonExtractor
    abstract class SingletonExtractor {
      def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Option[Term]
    }

    val Refined: RefinedExtractor
    abstract class RefinedExtractor {
      def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Option[(TypeTree, List[Definition])]
    }

    val Applied: AppliedExtractor
    abstract class AppliedExtractor {
      def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Option[(TypeTree, List[TypeOrBoundsTree])]
    }

    val Annotated: AnnotatedExtractor
    abstract class AnnotatedExtractor {
      def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Option[(TypeTree, Term)]
    }

    val And: AndExtractor
    abstract class AndExtractor {
      def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Option[(TypeTree, TypeTree)]
    }

    val Or: OrExtractor
    abstract class OrExtractor {
      def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Option[(TypeTree, TypeTree)]
    }

    val MatchType: MatchTypeExtractor
    abstract class MatchTypeExtractor {
      def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Option[(Option[TypeTree], TypeTree, List[TypeCaseDef])]
    }

    val ByName: ByNameExtractor
    abstract class ByNameExtractor {
      def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Option[TypeTree]
    }

    val TypeLambdaTree: TypeLambdaTreeExtractor
    abstract class TypeLambdaTreeExtractor {
      def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Option[(List[TypeDef], TypeOrBoundsTree)]
    }

    val Bind: BindExtractor
    abstract class BindExtractor{
      def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Option[(String, TypeBoundsTree)]
    }

    val Block: BlockExtractor
    abstract class BlockExtractor{
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

  /** TypeBoundsTree containing an inferred type bounds */
  val SyntheticBounds: SyntheticBoundsExtractor
  abstract class SyntheticBoundsExtractor {
    /** Matches a TypeBoundsTree containing inferred type bounds */
    def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Boolean
  }

  implicit def typeTreeAsParent(term: TypeTree): TermOrTypeTree
}
