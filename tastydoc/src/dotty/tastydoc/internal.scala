package dotty.tastydoc

import comment.Comment
import references._
// import dotty.tools.dotc.core.Symbols.{ Symbol, NoSymbol }

//TODO: why var? why def and var have same name?

object internal {

  final case class PackageImpl(
    // var symbol: Symbol,
    var annotations: List[String],
    var name: String,
    var members: List[Entity],
    var path: List[String],
    var superTypes: List[MaterializableLink] = Nil,
    var comment: Option[Comment] = None,
    var parent: Option[Entity] = None
  ) extends Package

  object EmptyPackage {
    def apply(path: List[String], name: String): PackageImpl = {
      // PackageImpl(NoSymbol, Nil, name, Nil, path)
      PackageImpl(Nil, name, Nil, path)
    }
  }

  final case class TypeAliasImpl (
    // symbol: Symbol,
    annotations: List[String],
    modifiers: List[String],
    name: String,
    path: List[String],
    alias: Option[Reference],
    typeParams: List[String] = Nil,
    var comment: Option[Comment] = None,
    var parent: Option[Entity] = None
  ) extends TypeAlias

  final case class ClassImpl(
    // symbol: Symbol,
    annotations: List[String],
    name: String,
    members: List[Entity],
    modifiers: List[String],
    path: List[String],
    typeParams: List[String] = Nil,
    constructors: List[List[ParamList]] = Nil,
    superTypes: List[MaterializableLink] = Nil,
    var comment: Option[Comment] = None,
    var companionPath: List[String] = Nil,
    var parent: Option[Entity] = None
  ) extends Class

  final case class CaseClassImpl(
    // symbol: Symbol,
    annotations: List[String],
    name: String,
    members: List[Entity],
    modifiers: List[String],
    path: List[String],
    typeParams: List[String] = Nil,
    constructors: List[List[ParamList]] = Nil,
    superTypes: List[MaterializableLink] = Nil,
    var comment: Option[Comment] = None,
    var companionPath: List[String] = Nil,
    var parent: Option[Entity] = None
  ) extends CaseClass

  final case class TraitImpl(
    // symbol: Symbol,
    annotations: List[String],
    name: String,
    members: List[Entity],
    modifiers: List[String],
    path: List[String],
    typeParams: List[String] = Nil,
    traitParams: List[ParamList] = Nil,
    superTypes: List[MaterializableLink] = Nil,
    var comment: Option[Comment] = None,
    var companionPath: List[String] = Nil,
    var parent: Option[Entity] = None
  ) extends Trait

  final case class ObjectImpl(
    // symbol: Symbol,
    annotations: List[String],
    name: String,
    members: List[Entity],
    private val mods: List[String],
    path: List[String],
    superTypes: List[MaterializableLink] = Nil,
    var comment: Option[Comment] = None,
    var companionPath: List[String] = Nil,
    var parent: Option[Entity] = None
  ) extends Object {
    def modifiers: List[String] = mods.filterNot(_ == "final")
  }

  final case class DefImpl(
    // symbol: Symbol,
    annotations: List[String],
    name: String,
    modifiers: List[String],
    path: List[String],
    returnValue: Reference,
    typeParams: List[String] = Nil,
    paramLists: List[ParamList] = Nil,
    var comment: Option[Comment] = None,
    implicitlyAddedFrom: Option[Reference] = None,
    var parent: Option[Entity] = None
  ) extends Def

  final case class ValImpl(
    // symbol: Symbol,
    annotations: List[String],
    name: String,
    modifiers: List[String],
    path: List[String],
    returnValue: Reference,
    kind: String,
    var comment: Option[Comment] = None,
    implicitlyAddedFrom: Option[Reference] = None,
    var parent: Option[Entity] = None
  ) extends Val

  final case class ParamListImpl(
    list: List[NamedReference],
    isImplicit: Boolean
  ) extends ParamList

  final case class ImportImpl(
    var annotations: List[String],
    var name: String,
    var path: List[String],
    var comment: Option[Comment] = None,
    var parent: Option[Entity] = None
  ) extends Import
}
