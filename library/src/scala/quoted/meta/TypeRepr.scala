package scala.quoted.meta

import scala.quoted.Type

trait TypeRepr private[meta] extends AnyRef {
  type Self <: TypeRepr

  // TODO (KR) : functions

}
object TypeRepr {

  def api(using meta: Meta): Meta.TypeReprAPI = meta.internal.typeRepr
  given Meta => Conversion[TypeRepr.type, Meta.TypeReprAPI] = _.api

}

trait NamedType private[meta] extends TypeRepr {
  override type Self <: NamedType

  // TODO (KR) : functions

}
object NamedType {

  def api(using meta: Meta): Meta.NamedTypeAPI = meta.internal.namedType
  given Meta => Conversion[NamedType.type, Meta.NamedTypeAPI] = _.api

}

trait TermRef private[meta] extends NamedType {
  override type Self <: TermRef

  // TODO (KR) : functions

}
object TermRef {

  def api(using meta: Meta): Meta.TermRefAPI = meta.internal.termRef
  given Meta => Conversion[TermRef.type, Meta.TermRefAPI] = _.api

}

trait TypeRef private[meta] extends NamedType {
  override type Self <: TypeRef

  // TODO (KR) : functions

}
object TypeRef {

  def api(using meta: Meta): Meta.TypeRefAPI = meta.internal.typeRef
  given Meta => Conversion[TypeRef.type, Meta.TypeRefAPI] = _.api

}

trait ConstantType private[meta] extends TypeRepr {
  override type Self <: ConstantType

  // TODO (KR) : functions

}
object ConstantType {

  def api(using meta: Meta): Meta.ConstantTypeAPI = meta.internal.constantType
  given Meta => Conversion[ConstantType.type, Meta.ConstantTypeAPI] = _.api

}

trait SuperType private[meta] extends TypeRepr {
  override type Self <: SuperType

  // TODO (KR) : functions

}
object SuperType {

  def api(using meta: Meta): Meta.SuperTypeAPI = meta.internal.superType
  given Meta => Conversion[SuperType.type, Meta.SuperTypeAPI] = _.api

}

trait Refinement private[meta] extends TypeRepr {
  override type Self <: Refinement

  // TODO (KR) : functions

}
object Refinement {

  def api(using meta: Meta): Meta.RefinementAPI = meta.internal.refinement
  given Meta => Conversion[Refinement.type, Meta.RefinementAPI] = _.api

}

trait AppliedType private[meta] extends TypeRepr {
  override type Self <: AppliedType

  // TODO (KR) : functions

}
object AppliedType {

  def api(using meta: Meta): Meta.AppliedTypeAPI = meta.internal.appliedType
  given Meta => Conversion[AppliedType.type, Meta.AppliedTypeAPI] = _.api

}

trait AnnotatedType private[meta] extends TypeRepr {
  override type Self <: AnnotatedType

  // TODO (KR) : functions

}
object AnnotatedType {

  def api(using meta: Meta): Meta.AnnotatedTypeAPI = meta.internal.annotatedType
  given Meta => Conversion[AnnotatedType.type, Meta.AnnotatedTypeAPI] = _.api

}

trait AndOrType private[meta] extends TypeRepr {
  override type Self <: AndOrType

  // TODO (KR) : functions

}
object AndOrType {

  def api(using meta: Meta): Meta.AndOrTypeAPI = meta.internal.andOrType
  given Meta => Conversion[AndOrType.type, Meta.AndOrTypeAPI] = _.api

}

trait AndType private[meta] extends AndOrType {
  override type Self <: AndType

  // TODO (KR) : functions

}
object AndType {

  def api(using meta: Meta): Meta.AndTypeAPI = meta.internal.andType
  given Meta => Conversion[AndType.type, Meta.AndTypeAPI] = _.api

}

trait OrType private[meta] extends AndOrType {
  override type Self <: OrType

  // TODO (KR) : functions

}
object OrType {

  def api(using meta: Meta): Meta.OrTypeAPI = meta.internal.orType
  given Meta => Conversion[OrType.type, Meta.OrTypeAPI] = _.api

}

trait MatchType private[meta] extends TypeRepr {
  override type Self <: MatchType

  // TODO (KR) : functions

}
object MatchType {

  def api(using meta: Meta): Meta.MatchTypeAPI = meta.internal.matchType
  given Meta => Conversion[MatchType.type, Meta.MatchTypeAPI] = _.api

}

trait ByNameType private[meta] extends TypeRepr {
  override type Self <: ByNameType

  // TODO (KR) : functions

}
object ByNameType {

  def api(using meta: Meta): Meta.ByNameTypeAPI = meta.internal.byNameType
  given Meta => Conversion[ByNameType.type, Meta.ByNameTypeAPI] = _.api

}

trait ParamRef private[meta] extends TypeRepr {
  override type Self <: ParamRef

  // TODO (KR) : functions

}
object ParamRef {

  def api(using meta: Meta): Meta.ParamRefAPI = meta.internal.paramRef
  given Meta => Conversion[ParamRef.type, Meta.ParamRefAPI] = _.api

}

trait Self private[meta] extends TypeRepr {
  override type Self <: scala.quoted.meta.Self

  // TODO (KR) : functions

}
object Self {

  def api(using meta: Meta): Meta.SelfAPI = meta.internal.self
  given Meta => Conversion[Self.type, Meta.SelfAPI] = _.api

}

trait RecursiveThis private[meta] extends TypeRepr {
  override type Self <: RecursiveThis

  // TODO (KR) : functions

}
object RecursiveThis {

  def api(using meta: Meta): Meta.RecursiveThisAPI = meta.internal.recursiveThis
  given Meta => Conversion[RecursiveThis.type, Meta.RecursiveThisAPI] = _.api

}

trait RecursiveType private[meta] extends TypeRepr {
  override type Self <: RecursiveType

  // TODO (KR) : functions

}
object RecursiveType {

  def api(using meta: Meta): Meta.RecursiveTypeAPI = meta.internal.recursiveType
  given Meta => Conversion[RecursiveType.type, Meta.RecursiveTypeAPI] = _.api

}

trait LambdaType private[meta] extends TypeRepr {
  override type Self <: LambdaType

  // TODO (KR) : functions

}
object LambdaType {

  def api(using meta: Meta): Meta.LambdaTypeAPI = meta.internal.lambdaType
  given Meta => Conversion[LambdaType.type, Meta.LambdaTypeAPI] = _.api

}

trait MethodOrPoly private[meta] extends LambdaType {
  override type Self <: MethodOrPoly

  // TODO (KR) : functions

}
object MethodOrPoly {

  def api(using meta: Meta): Meta.MethodOrPolyAPI = meta.internal.methodOrPoly
  given Meta => Conversion[MethodOrPoly.type, Meta.MethodOrPolyAPI] = _.api

}

trait MethodType private[meta] extends MethodOrPoly {
  override type Self <: MethodType

  // TODO (KR) : functions

}
object MethodType {

  def api(using meta: Meta): Meta.MethodTypeAPI = meta.internal.methodType
  given Meta => Conversion[MethodType.type, Meta.MethodTypeAPI] = _.api

}

trait PolyType private[meta] extends MethodOrPoly {
  override type Self <: PolyType

  // TODO (KR) : functions

}
object PolyType {

  def api(using meta: Meta): Meta.PolyTypeAPI = meta.internal.polyType
  given Meta => Conversion[PolyType.type, Meta.PolyTypeAPI] = _.api

}

trait TypeLambda private[meta] extends LambdaType {
  override type Self <: TypeLambda

  // TODO (KR) : functions

}
object TypeLambda {

  def api(using meta: Meta): Meta.TypeLambdaAPI = meta.internal.typeLambda
  given Meta => Conversion[TypeLambda.type, Meta.TypeLambdaAPI] = _.api

}

trait MatchCase private[meta] extends TypeRepr {
  override type Self <: MatchCase

  // TODO (KR) : functions

}
object MatchCase {

  def api(using meta: Meta): Meta.MatchCaseAPI = meta.internal.matchCase
  given Meta => Conversion[MatchCase.type, Meta.MatchCaseAPI] = _.api

}

trait TypeBounds private[meta] extends TypeRepr {
  override type Self <: TypeBounds

  // TODO (KR) : functions

}
object TypeBounds {

  def api(using meta: Meta): Meta.TypeBoundsAPI = meta.internal.typeBounds
  given Meta => Conversion[TypeBounds.type, Meta.TypeBoundsAPI] = _.api

}

trait NoPrefix private[meta] extends TypeRepr {
  override type Self <: NoPrefix

  // TODO (KR) : functions

}
object NoPrefix {

  def api(using meta: Meta): Meta.NoPrefixAPI = meta.internal.noPrefix
  given Meta => Conversion[NoPrefix.type, Meta.NoPrefixAPI] = _.api

}

trait FlexibleType private[meta] extends TypeRepr {
  override type Self <: FlexibleType

  // TODO (KR) : functions

}
object FlexibleType {

  def api(using meta: Meta): Meta.FlexibleTypeAPI = meta.internal.flexibleType
  given Meta => Conversion[FlexibleType.type, Meta.FlexibleTypeAPI] = _.api

}
