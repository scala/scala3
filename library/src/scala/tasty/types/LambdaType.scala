package scala.tasty
package types

trait LambdaType[ParamName <: names.Name, ParamInfo <: MaybeType] extends Type

trait MethodType extends LambdaType[names.TermName, Type] {
  def isImplicit: Boolean
  def isErased: Boolean
}
object MethodType {
  type Data = (List[names.TermName], List[Type], Type)
  def unapply(arg: MaybeType)(implicit ext: Extractor): Option[Data] = ext.unapplyMethodType(arg)
}

trait PolyType extends LambdaType[names.TypeName, TypeBounds]
object PolyType {
  type Data = (List[names.TypeName], List[TypeBounds], Type)
  def unapply(arg: MaybeType)(implicit ext: Extractor): Option[Data] = ext.unapplyPolyType(arg)
}

trait TypeLambda extends LambdaType[names.TypeName, TypeBounds]
object TypeLambda {
  type Data = (List[names.TypeName], List[TypeBounds], Type)
  def unapply(arg: MaybeType)(implicit ext: Extractor): Option[Data] = ext.unapplyTypeLambda(arg)
}
