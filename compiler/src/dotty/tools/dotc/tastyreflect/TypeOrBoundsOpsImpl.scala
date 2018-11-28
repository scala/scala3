package dotty.tools.dotc.tastyreflect

import dotty.tools.dotc.core.{Contexts, Names, Types}

trait TypeOrBoundsOpsImpl extends scala.tasty.reflect.TypeOrBoundsOps with CoreImpl {

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

    object IsConstantType extends IsConstantTypeModule {
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[ConstantType] = tpe match {
        case tpe: Types.ConstantType => Some(tpe)
        case _ => None
      }
    }

    def ConstantTypeDeco(x: ConstantType): ConstantTypeAPI = new ConstantTypeAPI {
      def value(implicit ctx: Context): Any = x.value
    }

    object ConstantType extends ConstantTypeExtractor {
      def unapply(x: TypeOrBounds)(implicit ctx: Context): Option[Constant] = x match {
        case Types.ConstantType(value) => Some(value)
        case _ => None
      }
    }

    object IsSymRef extends IsSymRefModule {
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[SymRef] = tpe match {
        case tp: Types.NamedType =>
          tp.designator match {
            case sym: Symbol => Some(tp)
            case _ => None
          }
        case _ => None
      }
    }

    def SymRefDeco(x: SymRef): SymRefAPI = new SymRefAPI {
      def qualifier(implicit ctx: Context): TypeOrBounds = x.prefix
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

    object IsTermRef extends IsTermRefModule {
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[TermRef] = tpe match {
        case tpe: Types.NamedType =>
          tpe.designator match {
            case name: Names.TermName => Some(tpe)
            case _ => None
          }
        case _ => None
      }
    }

    def TermRefDeco(x: TermRef): TermRefAPI = new TermRefAPI {
      def qualifier(implicit ctx: Context): TypeOrBounds = x.prefix
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

    object IsTypeRef extends IsTypeRefModule {
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[TypeRef] = tpe match {
        case tpe: Types.NamedType =>
          tpe.designator match {
            case name: Names.TypeName => Some(tpe)
            case _ => None
          }
        case _ => None
      }
    }

    def TypeRefDeco(x: TypeRef): TypeRefAPI = new TypeRefAPI {
      def name(implicit ctx: Context): String = x.name.toString
      def qualifier(implicit ctx: Context): TypeOrBounds = x.prefix
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

    object IsSuperType extends IsSuperTypeModule {
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[SuperType] = tpe match {
        case tpe: Types.SuperType => Some(tpe)
        case _ => None
      }
    }

    def SuperTypeDeco(x: SuperType): SuperTypeAPI = new SuperTypeAPI {
      def thistpe(implicit ctx: Context): Type = x.thistpe
      def supertpe(implicit ctx: Context): Type = x.supertpe
    }

    object SuperType extends SuperTypeExtractor {
      def unapply(x: TypeOrBounds)(implicit ctx: Context): Option[(Type, Type)] = x match {
        case Types.SuperType(thistpe, supertpe) => Some(thistpe, supertpe)
        case _ => None
      }
    }

    object IsRefinement extends IsRefinementModule {
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[Refinement] = tpe match {
        case tpe: Types.RefinedType => Some(tpe)
        case _ => None
      }
    }

    def RefinementDeco(x: Refinement): RefinementAPI = new RefinementAPI {
      def parent(implicit ctx: Context): Type = x.parent
      def name(implicit ctx: Context): String = x.refinedName.toString
      def info(implicit ctx: Context): TypeOrBounds = x.refinedInfo
    }

    object Refinement extends RefinementExtractor {
      def unapply(x: TypeOrBounds)(implicit ctx: Context): Option[(Type, String, TypeOrBounds /* Type | TypeBounds */)] = x match {
        case Types.RefinedType(parent, name, info) => Some(parent, name.toString, info)
        case _ => None
      }
    }

    object IsAppliedType extends IsAppliedTypeModule {
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[AppliedType] = tpe match {
        case tpe: Types.AppliedType => Some(tpe)
        case _ => None
      }
    }

    def AppliedTypeDeco(x: AppliedType): AppliedTypeAPI = new AppliedTypeAPI {
      def tycon(implicit ctx: Context): Type = x.tycon
      def args(implicit ctx: Context): List[TypeOrBounds] = x.args
    }

    object AppliedType extends AppliedTypeExtractor {
      def unapply(x: TypeOrBounds)(implicit ctx: Context): Option[(Type, List[TypeOrBounds /* Type | TypeBounds */])] = x match {
        case Types.AppliedType(tycon, args) => Some((tycon.stripTypeVar, args.map(_.stripTypeVar)))
        case _ => None
      }
    }

    object IsAnnotatedType extends IsAnnotatedTypeModule {
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[AnnotatedType] = tpe match {
        case tpe: Types.AnnotatedType => Some(tpe)
        case _ => None
      }
    }

    def AnnotatedTypeDeco(x: AnnotatedType): AnnotatedTypeAPI = new AnnotatedTypeAPI {
      def underlying(implicit ctx: Context): Type = x.underlying.stripTypeVar
      def annot(implicit ctx: Context): Term = x.annot.tree
    }

    object AnnotatedType extends AnnotatedTypeExtractor {
      def unapply(x: TypeOrBounds)(implicit ctx: Context): Option[(Type, Term)] = x match {
        case Types.AnnotatedType(underlying, annot) => Some((underlying.stripTypeVar, annot.tree))
        case _ => None
      }
    }

    object IsAndType extends IsAndTypeModule {
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[AndType] = tpe match {
        case tpe: Types.AndType => Some(tpe)
        case _ => None
      }
    }

    def AndTypeDeco(x: AndType): AndTypeAPI = new AndTypeAPI {
      def left(implicit ctx: Context): Type = x.tp1.stripTypeVar
      def right(implicit ctx: Context): Type = x.tp2.stripTypeVar
    }

    object AndType extends AndTypeExtractor {
      def unapply(x: TypeOrBounds)(implicit ctx: Context): Option[(Type, Type)] = x match {
        case Types.AndType(left, right) => Some(left.stripTypeVar, right.stripTypeVar)
        case _ => None
      }
    }

    object IsOrType extends IsOrTypeModule {
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[OrType] = tpe match {
        case tpe: Types.OrType => Some(tpe)
        case _ => None
      }
    }

    def OrTypeDeco(x: OrType): OrTypeAPI = new OrTypeAPI {
      def left(implicit ctx: Context): Type = x.tp1
      def right(implicit ctx: Context): Type = x.tp2
    }

    object OrType extends OrTypeExtractor {
      def unapply(x: TypeOrBounds)(implicit ctx: Context): Option[(Type, Type)] = x match {
        case Types.OrType(left, right) => Some(left.stripTypeVar, right.stripTypeVar)
        case _ => None
      }
    }

    object IsMatchType extends IsMatchTypeModule {
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[MatchType] = tpe match {
        case tpe: Types.MatchType => Some(tpe)
        case _ => None
      }
    }

    def MatchTypeDeco(x: MatchType): MatchTypeAPI = new MatchTypeAPI {
      def bound(implicit ctx: Context): Type = x.bound
      def scrutinee(implicit ctx: Context): Type = x.scrutinee
      def cases(implicit ctx: Context): List[Type] = x.cases
    }

    object MatchType extends MatchTypeExtractor {
      def unapply(x: TypeOrBounds)(implicit ctx: Context): Option[(Type, Type, List[Type])] = x match {
        case Types.MatchType(bound, scrutinee, cases) => Some((bound, scrutinee, cases))
        case _ => None
      }
    }

    object IsByNameType extends IsByNameTypeModule {
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[ByNameType] = tpe match {
        case tpe: Types.ExprType => Some(tpe)
        case _ => None
      }
    }

    def ByNameTypeDeco(x: ByNameType): ByNameTypeAPI = new ByNameTypeAPI {
      def underlying(implicit ctx: Context): Type = x.resType.stripTypeVar
    }

    object ByNameType extends ByNameTypeExtractor {
      def unapply(x: TypeOrBounds)(implicit ctx: Context): Option[Type] = x match {
        case Types.ExprType(resType) => Some(resType.stripTypeVar)
        case _ => None
      }
    }

    object IsParamRef extends IsParamRefModule {
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[ParamRef] = tpe match {
        case tpe: Types.TypeParamRef => Some(tpe)
        case tpe: Types.TermParamRef => Some(tpe)
        case _ => None
      }
    }

    def ParamRefDeco(x: ParamRef): ParamRefAPI = new ParamRefAPI {
      def binder(implicit ctx: Context): LambdaType[TypeOrBounds] =
        x.binder.asInstanceOf[LambdaType[TypeOrBounds]] // Cast to tpd
      def paramNum(implicit ctx: Context): Int = x.paramNum
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

    object IsThisType extends IsThisTypeModule {
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[ThisType] = tpe match {
        case tpe: Types.ThisType => Some(tpe)
        case _ => None
      }
    }

    def ThisTypeDeco(x: ThisType): ThisTypeAPI = new ThisTypeAPI {
      def underlying(implicit ctx: Context): Type = x.underlying
    }

    object ThisType extends ThisTypeExtractor {
      def unapply(x: TypeOrBounds)(implicit ctx: Context): Option[Type] = x match {
        case Types.ThisType(tp) => Some(tp)
        case _ => None
      }
    }

    object IsRecursiveThis extends IsRecursiveThisModule {
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[RecursiveThis] = tpe match {
        case tpe: Types.RecThis => Some(tpe)
        case _ => None
      }
    }

    def RecursiveThisDeco(x: RecursiveThis): RecursiveThisAPI = new RecursiveThisAPI {
      def binder(implicit ctx: Context): RecursiveType = x.binder
    }

    object RecursiveThis extends RecursiveThisExtractor {
      def unapply(x: TypeOrBounds)(implicit ctx: Context): Option[RecursiveType] = x match {
        case Types.RecThis(binder) => Some(binder)
        case _ => None
      }
    }

    object IsRecursiveType extends IsRecursiveTypeModule {
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[RecursiveType] = tpe match {
        case tpe: Types.RecType => Some(tpe)
        case _ => None
      }
    }

    def RecursiveTypeDeco(x: RecursiveType): RecursiveTypeAPI = new RecursiveTypeAPI {
      def underlying(implicit ctx: Context): Type = x.underlying.stripTypeVar
    }

    object RecursiveType extends RecursiveTypeExtractor {
      def unapply(x: TypeOrBounds)(implicit ctx: Context): Option[Type] = x match {
        case tp: Types.RecType => Some(tp.underlying.stripTypeVar)
        case _ => None
      }
    }

    object IsMethodType extends IsMethodTypeModule {
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[MethodType] = tpe match {
        case tpe: Types.MethodType => Some(tpe)
        case _ => None
      }
    }

    def MethodTypeDeco(x: MethodType): MethodTypeAPI = new MethodTypeAPI {
      def paramNames(implicit ctx: Context): List[String] = x.paramNames.map(_.toString)
      def paramTypes(implicit ctx: Context): List[Type] = x.paramInfos
      def resType(implicit ctx: Context): Type = x.resType
    }

    object MethodType extends MethodTypeExtractor {
      def unapply(x: TypeOrBounds)(implicit ctx: Context): Option[(List[String], List[Type], Type)] = x match {
        case x: MethodType => Some(x.paramNames.map(_.toString), x.paramInfos, x.resType)
        case _ => None
      }
    }

    object IsPolyType extends IsPolyTypeModule {
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[PolyType] = tpe match {
        case tpe: Types.PolyType => Some(tpe)
        case _ => None
      }
    }

    def PolyTypeDeco(x: PolyType): PolyTypeAPI = new PolyTypeAPI {
      def paramNames(implicit ctx: Contexts.Context): List[String] = x.paramNames.map(_.toString)
      def paramBounds(implicit ctx: Contexts.Context): List[TypeBounds] = x.paramInfos
      def resType(implicit ctx: Contexts.Context): Type = x.resType
    }

    object PolyType extends PolyTypeExtractor {
      def unapply(x: TypeOrBounds)(implicit ctx: Context): Option[(List[String], List[TypeBounds], Type)] = x match {
        case x: PolyType => Some(x.paramNames.map(_.toString), x.paramInfos, x.resType)
        case _ => None
      }
    }

    object IsTypeLambda extends IsTypeLambdaModule {
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[TypeLambda] = tpe match {
        case tpe: Types.TypeLambda => Some(tpe)
        case _ => None
      }
    }

    def TypeLambdaDeco(x: TypeLambda): TypeLambdaAPI = new TypeLambdaAPI {
      def paramNames(implicit ctx: Contexts.Context): List[String] = x.paramNames.map(_.toString)
      def paramBounds(implicit ctx: Contexts.Context): List[TypeBounds] = x.paramInfos
      def resType(implicit ctx: Contexts.Context): Type = x.resType
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
