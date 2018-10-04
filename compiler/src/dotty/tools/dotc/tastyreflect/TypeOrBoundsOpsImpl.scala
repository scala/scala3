package dotty.tools.dotc.tastyreflect

import dotty.tools.dotc.core.{Names, Types}

trait TypeOrBoundsOpsImpl extends scala.tasty.reflect.TypeOrBoundsOps with TastyCoreImpl {

  // ===== Types ====================================================

  def TypeDeco(tpe: Type): TypeAPI = new TypeAPI {
    def =:=(other: Type)(implicit ctx: Context): Boolean = tpe =:= other
    def <:<(other: Type)(implicit ctx: Context): Boolean = tpe <:< other
  }

  def MethodTypeDeco(tpe: MethodType): MethodTypeAPI = new MethodTypeAPI {
    def isErased: Boolean = tpe.isErasedMethod
    def isImplicit: Boolean = tpe.isImplicitMethod
    def paramNames(implicit ctx: Context): List[String] = tpe.paramNames.map(_.toString)
    def paramTypes(implicit ctx: Context): List[Type] = tpe.paramInfos
    def resultTpe(implicit ctx: Context): Type = tpe.resType
  }

  def PolyTypeDeco(tpe: Types.PolyType): PolyTypeAPI = new PolyTypeAPI {
    def paramNames(implicit ctx: Context): List[String] = tpe.paramNames.map(_.toString)
    def paramTypes(implicit ctx: Context): List[TypeBounds] = tpe.paramInfos
    def resultTpe(implicit ctx: Context): Type = tpe.resType
  }

  def TypeLambdaDeco(tpe: TypeLambda): TypeLambdaAPI = new TypeLambdaAPI {
    def paramNames(implicit ctx: Context): List[String] = tpe.paramNames.map(_.toString)
    def paramTypes(implicit ctx: Context): List[TypeBounds] = tpe.paramInfos
    def resultTpe(implicit ctx: Context): Type = tpe.resType
  }

  object IsType extends IsTypeExtractor {
    def unapply(x: TypeOrBounds)(implicit ctx: Context): Option[Type] = x match {
      case x: TypeBounds => None
      case x if x == Types.NoPrefix => None
      case _ => Some(x)
    }
  }

  object Type extends TypeModule {

    object ConstantType extends ConstantTypeExtractor {
      def unapply(x: TypeOrBounds)(implicit ctx: Context): Option[Constant] = x match {
        case Types.ConstantType(value) => Some(value)
        case _ => None
      }
    }

    object SymRef extends SymRefExtractor {
      def unapply(x: TypeOrBounds)(implicit ctx: Context): Option[(Symbol, TypeOrBounds /* Type | NoPrefix */)] = x  match {
        case tp: Types.NamedType =>
          tp.designator match {
            case sym: Symbol => Some((sym, tp.prefix))
            case _ => None
          }
        case _ => None
      }
    }

    object TermRef extends TermRefExtractor {
      def unapply(x: TypeOrBounds)(implicit ctx: Context): Option[(String, TypeOrBounds /* Type | NoPrefix */)] = x match {
        case tp: Types.NamedType =>
          tp.designator match {
            case name: Names.TermName => Some(name.toString, tp.prefix)
            case _ => None
          }
        case _ => None
      }
    }

    object TypeRef extends TypeRefExtractor {
      def unapply(x: TypeOrBounds)(implicit ctx: Context): Option[(String, TypeOrBounds /* Type | NoPrefix */)] = x match {
        case tp: Types.NamedType =>
          tp.designator match {
            case name: Names.TypeName => Some(name.toString, tp.prefix)
            case _ => None
          }
        case _ => None
      }
    }

    object SuperType extends SuperTypeExtractor {
      def unapply(x: TypeOrBounds)(implicit ctx: Context): Option[(Type, Type)] = x match {
        case Types.SuperType(thistpe, supertpe) => Some(thistpe, supertpe)
        case _ => None
      }
    }

    object Refinement extends RefinementExtractor {
      def unapply(x: TypeOrBounds)(implicit ctx: Context): Option[(Type, String, TypeOrBounds /* Type | TypeBounds */)] = x match {
        case Types.RefinedType(parent, name, info) => Some(parent, name.toString, info)
        case _ => None
      }
    }

    object AppliedType extends AppliedTypeExtractor {
      def unapply(x: TypeOrBounds)(implicit ctx: Context): Option[(Type, List[TypeOrBounds /* Type | TypeBounds */])] = x match {
        case Types.AppliedType(tycon, args) => Some((tycon.stripTypeVar, args.map(_.stripTypeVar)))
        case _ => None
      }
    }

    object AnnotatedType extends AnnotatedTypeExtractor {
      def unapply(x: TypeOrBounds)(implicit ctx: Context): Option[(Type, Term)] = x match {
        case Types.AnnotatedType(underlying, annot) => Some((underlying.stripTypeVar, annot.tree))
        case _ => None
      }
    }

    object AndType extends AndTypeExtractor {
      def unapply(x: TypeOrBounds)(implicit ctx: Context): Option[(Type, Type)] = x match {
        case Types.AndType(left, right) => Some(left.stripTypeVar, right.stripTypeVar)
        case _ => None
      }
    }

    object OrType extends OrTypeExtractor {
      def unapply(x: TypeOrBounds)(implicit ctx: Context): Option[(Type, Type)] = x match {
        case Types.OrType(left, right) => Some(left.stripTypeVar, right.stripTypeVar)
        case _ => None
      }
    }

    object ByNameType extends ByNameTypeExtractor {
      def unapply(x: TypeOrBounds)(implicit ctx: Context): Option[Type] = x match {
        case Types.ExprType(resType) => Some(resType.stripTypeVar)
        case _ => None
      }
    }

    object ParamRef extends ParamRefExtractor {
      def unapply(x: TypeOrBounds)(implicit ctx: Context): Option[(LambdaType[TypeOrBounds], Int)] = x match {
        case Types.TypeParamRef(binder, idx) =>
          Some((
            binder.asInstanceOf[LambdaType[TypeOrBounds]], // Cast to tpd
            idx))
        case Types.TermParamRef(binder, idx) => Some((binder, idx))
        case _ => None
      }
    }

    object ThisType extends ThisTypeExtractor {
      def unapply(x: TypeOrBounds)(implicit ctx: Context): Option[Type] = x match {
        case Types.ThisType(tp) => Some(tp)
        case _ => None
      }
    }

    object RecursiveThis extends RecursiveThisExtractor {
      def unapply(x: TypeOrBounds)(implicit ctx: Context): Option[RecursiveType] = x match {
        case Types.RecThis(binder) => Some(binder)
        case _ => None
      }
    }

    object RecursiveType extends RecursiveTypeExtractor {
      def unapply(x: TypeOrBounds)(implicit ctx: Context): Option[Type] = x match {
        case tp: Types.RecType => Some(tp.underlying.stripTypeVar)
        case _ => None
      }
    }

    object MethodType extends MethodTypeExtractor {
      def unapply(x: TypeOrBounds)(implicit ctx: Context): Option[(List[String], List[Type], Type)] = x match {
        case x: MethodType => Some(x.paramNames.map(_.toString), x.paramInfos, x.resType)
        case _ => None
      }
    }

    object PolyType extends PolyTypeExtractor {
      def unapply(x: TypeOrBounds)(implicit ctx: Context): Option[(List[String], List[TypeBounds], Type)] = x match {
        case x: PolyType => Some(x.paramNames.map(_.toString), x.paramInfos, x.resType)
        case _ => None
      }
    }

    object TypeLambda extends TypeLambdaExtractor {
      def unapply(x: TypeOrBounds)(implicit ctx: Context): Option[(List[String], List[TypeBounds], Type)] = x match {
        case x: TypeLambda => Some(x.paramNames.map(_.toString), x.paramInfos, x.resType)
        case _ => None
      }
    }

  }

  // ----- TypeBounds ------------------------------------------------

  object IsTypeBounds extends IsTypeBoundsExtractor {
    def unapply(x: TypeOrBounds)(implicit ctx: Context): Option[TypeBounds] = x match {
      case x: TypeBounds => Some(x)
      case _ => None
    }
  }

  object TypeBounds extends TypeBoundsExtractor {
    def unapply(x: TypeOrBounds)(implicit ctx: Context): Option[(Type, Type)] = x match {
      case x: TypeBounds => Some(x.lo, x.hi)
      case _ => None
    }
  }

  def TypeBoundsDeco(tpe: TypeBounds): TypeBoundsAPI = new TypeBoundsAPI {
    def low(implicit ctx: Context): Type = tpe.lo
    def hi(implicit ctx: Context): Type = tpe.hi
  }

  // ----- NoPrefix --------------------------------------------------

  object NoPrefix extends NoPrefixExtractor {
    def unapply(x: TypeOrBounds)(implicit ctx: Context): Boolean = x == Types.NoPrefix
  }

}
