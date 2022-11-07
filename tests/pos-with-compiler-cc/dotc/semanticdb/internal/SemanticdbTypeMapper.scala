package dotty.tools.dotc.semanticdb.internal

abstract class SemanticdbTypeMapper[BaseType, CustomType] {
  def toCustom(base: BaseType): CustomType
  def toBase(custom: CustomType): BaseType
}

object SemanticdbTypeMapper {
  def apply[BaseType, CustomType](baseToCustom: BaseType => CustomType)(
      customToBase: CustomType => BaseType
  ): SemanticdbTypeMapper[BaseType, CustomType] =
    new SemanticdbTypeMapper[BaseType, CustomType] {
      def toCustom(base: BaseType): CustomType = baseToCustom(base)
      def toBase(custom: CustomType): BaseType = customToBase(custom)
    }
}

