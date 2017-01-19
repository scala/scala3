package dotty.tools.dottydoc
package model

import comment.Comment
import references._
import dotty.tools.dotc.core.Symbols.Symbol

object internal {

  trait Impl {
    var parent: Entity = NonEntity
  }

  final case class PackageImpl(
    symbol: Symbol,
    annotations: List[String],
    name: String,
    var members: List[Entity],
    path: List[String],
    superTypes: List[MaterializableLink] = Nil,
    var comment: Option[Comment] = None
  ) extends Package with Impl

  final case class TypeAliasImpl (
    symbol: Symbol,
    annotations: List[String],
    modifiers: List[String],
    name: String,
    path: List[String],
    alias: Option[Reference],
    var comment: Option[Comment] = None
  ) extends TypeAlias with Impl

  final case class ClassImpl(
    symbol: Symbol,
    annotations: List[String],
    name: String,
    members: List[Entity],
    modifiers: List[String],
    path: List[String],
    typeParams: List[String] = Nil,
    constructors: List[List[ParamList]] = Nil,
    superTypes: List[MaterializableLink] = Nil,
    var comment: Option[Comment] = None,
    var companionPath: List[String] = Nil
  ) extends Class with Impl

  final case class CaseClassImpl(
    symbol: Symbol,
    annotations: List[String],
    name: String,
    members: List[Entity],
    modifiers: List[String],
    path: List[String],
    typeParams: List[String] = Nil,
    constructors: List[List[ParamList]] = Nil,
    superTypes: List[MaterializableLink] = Nil,
    var comment: Option[Comment] = None,
    var companionPath: List[String] = Nil
  ) extends CaseClass with Impl

  final case class TraitImpl(
    symbol: Symbol,
    annotations: List[String],
    name: String,
    members: List[Entity],
    modifiers: List[String],
    path: List[String],
    typeParams: List[String] = Nil,
    traitParams: List[ParamList] = Nil,
    superTypes: List[MaterializableLink] = Nil,
    var comment: Option[Comment] = None,
    var companionPath: List[String] = Nil
  ) extends Trait with Impl

  final case class ObjectImpl(
    symbol: Symbol,
    annotations: List[String],
    name: String,
    members: List[Entity],
    modifiers: List[String],
    path: List[String],
    superTypes: List[MaterializableLink] = Nil,
    var comment: Option[Comment] = None,
    var companionPath: List[String] = Nil
  ) extends Object with Impl

  final case class DefImpl(
    symbol: Symbol,
    annotations: List[String],
    name: String,
    modifiers: List[String],
    path: List[String],
    returnValue: Reference,
    typeParams: List[String] = Nil,
    paramLists: List[ParamList] = Nil,
    var comment: Option[Comment] = None,
    implicitlyAddedFrom: Option[Reference] = None
  ) extends Def with Impl

  final case class ValImpl(
    symbol: Symbol,
    annotations: List[String],
    name: String,
    modifiers: List[String],
    path: List[String],
    returnValue: Reference,
    kind: String,
    var comment: Option[Comment] = None,
    implicitlyAddedFrom: Option[Reference] = None
  ) extends Val with Impl

  final case class ParamListImpl(
    list: List[NamedReference],
    isImplicit: Boolean
  ) extends ParamList
}
