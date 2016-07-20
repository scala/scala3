package dotty.tools.dottydoc
package model

import comment.{ Comment, MaterializableLink, Reference, NamedReference }

object internal {

  trait Impl {
    var parent: Entity = NonEntity
  }

  final case class PackageImpl(
    name: String,
    var members: List[Entity],
    path: List[String],
    var comment: Option[Comment] = None
  ) extends Package with Impl {
    def children: List[Entity with Members] =
      members.collect { case x: Entity with Members => x }
  }

  final case class ClassImpl(
    name: String,
    members: List[Entity],
    modifiers: List[String],
    path: List[String],
    var typeParams: List[String] = Nil,
    var superTypes: List[MaterializableLink] = Nil,
    var comment: Option[Comment] = None
  ) extends Class with Impl

  final case class CaseClassImpl(
    name: String,
    members: List[Entity],
    modifiers: List[String],
    path: List[String],
    var typeParams: List[String] = Nil,
    var superTypes: List[MaterializableLink] = Nil,
    var comment: Option[Comment] = None
  ) extends CaseClass with Impl

  final case class TraitImpl(
    name: String,
    members: List[Entity],
    modifiers: List[String],
    path: List[String],
    var typeParams: List[String] = Nil,
    var superTypes: List[MaterializableLink] = Nil,
    var comment: Option[Comment] = None
  ) extends Trait with Impl

  final case class ObjectImpl(
    name: String,
    members: List[Entity],
    modifiers: List[String],
    path: List[String],
    var superTypes: List[MaterializableLink] = Nil,
    var comment: Option[Comment] = None
  ) extends Object with Impl

  final case class DefImpl(
    name: String,
    modifiers: List[String],
    path: List[String],
    var returnValue: Reference,
    var typeParams: List[String] = Nil,
    var paramLists: List[ParamList] = Nil,
    var comment: Option[Comment] = None
  ) extends Def with Impl

  final case class ValImpl(
    name: String,
    modifiers: List[String],
    path: List[String],
    var returnValue: Reference,
    var comment: Option[Comment] = None
  ) extends Val with Impl

  final case class ParamListImpl(
    list: List[NamedReference],
    isImplicit: Boolean
  ) extends ParamList
}
