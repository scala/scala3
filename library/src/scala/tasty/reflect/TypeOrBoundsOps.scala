package scala.tasty
package reflect

trait TypeOrBoundsOps extends ReflectionCore {

  // ----- Types ----------------------------------------------------

  trait TypeAPI {
    def =:=(other: Type)(implicit ctx: Context): Boolean
    def <:<(other: Type)(implicit ctx: Context): Boolean
  }
  implicit def TypeDeco(tpe: Type): TypeAPI

  trait MethodTypeAPI {
    def isImplicit: Boolean
    def isErased: Boolean
    def paramNames(implicit ctx: Context): List[String]
    def paramTypes(implicit ctx: Context): List[Type]
    def resultTpe(implicit ctx: Context): Type
  }
  implicit def MethodTypeDeco(tpt: MethodType): MethodTypeAPI

  trait PolyTypeAPI {
    def paramNames(implicit ctx: Context): List[String]
    def paramTypes(implicit ctx: Context): List[TypeBounds]
    def resultTpe(implicit ctx: Context): Type
  }
  implicit def PolyTypeDeco(tpt: PolyType): PolyTypeAPI

  trait TypeLambdaAPI {
    def paramNames(implicit ctx: Context): List[String]
    def paramTypes(implicit ctx: Context): List[TypeBounds]
    def resultTpe(implicit ctx: Context): Type
  }
  implicit def TypeLambdaDeco(tpt: TypeLambda): TypeLambdaAPI

  val IsType: IsTypeExtractor
  abstract class IsTypeExtractor {
    def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[Type]
  }

  val Type: TypeModule
  abstract class TypeModule {

    val ConstantType: ConstantTypeExtractor
    abstract class ConstantTypeExtractor {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[Constant]
    }

    val SymRef: SymRefExtractor
    abstract class SymRefExtractor {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(Symbol, TypeOrBounds /* Type | NoPrefix */)]
    }

    val TermRef: TermRefExtractor
    abstract class TermRefExtractor {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(String, TypeOrBounds /* Type | NoPrefix */)]
    }

    val TypeRef: TypeRefExtractor
    abstract class TypeRefExtractor {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(String, TypeOrBounds /* Type | NoPrefix */)]
    }

    val SuperType: SuperTypeExtractor
    abstract class SuperTypeExtractor {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(Type, Type)]
    }

    val Refinement: RefinementExtractor
    abstract class RefinementExtractor {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(Type, String, TypeOrBounds /* Type | TypeBounds */)]
    }

    val AppliedType: AppliedTypeExtractor
    abstract class AppliedTypeExtractor {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(Type, List[TypeOrBounds /* Type | TypeBounds */])]
    }

    val AnnotatedType: AnnotatedTypeExtractor
    abstract class AnnotatedTypeExtractor {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(Type, Term)]
    }

    val AndType: AndTypeExtractor
    abstract class AndTypeExtractor {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(Type, Type)]
    }

    val OrType: OrTypeExtractor
    abstract class OrTypeExtractor {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(Type, Type)]
    }

    val ByNameType: ByNameTypeExtractor
    abstract class ByNameTypeExtractor {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[Type]
    }

    val ParamRef: ParamRefExtractor
    abstract class ParamRefExtractor {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(LambdaType[TypeOrBounds], Int)]
    }

    val ThisType: ThisTypeExtractor
    abstract class ThisTypeExtractor {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[Type]
    }

    val RecursiveThis: RecursiveThisExtractor
    abstract class RecursiveThisExtractor {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[RecursiveType]
    }

    val RecursiveType: RecursiveTypeExtractor
    abstract class RecursiveTypeExtractor {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[Type]
    }

    val MethodType: MethodTypeExtractor
    abstract class MethodTypeExtractor {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(List[String], List[Type], Type)]
    }

    val PolyType: PolyTypeExtractor
    abstract class PolyTypeExtractor {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(List[String], List[TypeBounds], Type)]
    }

    val TypeLambda: TypeLambdaExtractor
    abstract class TypeLambdaExtractor {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(List[String], List[TypeBounds], Type)]
    }

  }

  // ----- TypeBounds -----------------------------------------------

  val IsTypeBounds: IsTypeBoundsExtractor
  abstract class IsTypeBoundsExtractor {
    def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[TypeBounds]
  }

  val TypeBounds: TypeBoundsExtractor
  abstract class TypeBoundsExtractor {
    def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(Type, Type)]
  }

  trait TypeBoundsAPI {
    def low(implicit ctx: Context): Type
    def hi(implicit ctx: Context): Type
  }
  implicit def TypeBoundsDeco(bounds: TypeBounds): TypeBoundsAPI

  // ----- NoPrefix -------------------------------------------------

  val NoPrefix: NoPrefixExtractor
  abstract class NoPrefixExtractor {
    def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Boolean
  }

}
