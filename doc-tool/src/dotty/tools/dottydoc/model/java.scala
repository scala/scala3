package dotty.tools.dottydoc
package model

import comment._
import references._

import _root_.java.util.HashMap
import _root_.java.util.LinkedList

object java {
  import scala.collection.JavaConverters._
  import _root_.java.util.{ Optional => JOptional, Map => JMap }

  implicit class OptStr(val opt: Option[String]) extends AnyVal {
    def asJava = opt.getOrElse(null)
  }

  implicit class OptMap(val opt: Option[JMap[String, _]]) extends AnyVal {
    def asJava = opt.getOrElse(Map.empty.asJava)
  }

  implicit class JavaComment(val cmt: Comment) extends AnyVal {
    def asJava: JMap[String, _] = Map(
      "body"                    -> cmt.body,
      "short"                   -> cmt.short,
      "authors"                 -> cmt.authors.asJava,
      "see"                     -> cmt.see.asJava,
      "result"                  -> cmt.result.asJava,
      "throws"                  -> cmt.throws.asJava,
      "valueParams"             -> cmt.valueParams.asJava,
      "typeParams"              -> cmt.typeParams.asJava,
      "version"                 -> cmt.version.asJava,
      "since"                   -> cmt.since.asJava,
      "todo"                    -> cmt.todo.asJava,
      "deprecated"              -> cmt.deprecated.asJava,
      "note"                    -> cmt.note.asJava,
      "example"                 -> cmt.example.asJava,
      "constructor"             -> cmt.constructor.asJava,
      "group"                   -> cmt.group.asJava,
      "groupDesc"               -> cmt.groupDesc.asJava,
      "groupNames"              -> cmt.groupNames.asJava,
      "groupPrio"               -> cmt.groupPrio.asJava,
      "hideImplicitConversions" -> cmt.hideImplicitConversions.asJava
    ).asJava
  }

  implicit class JavaPackage(val ent: Package) extends AnyVal {
    def asJava(extras: Map[String, _] = Map.empty): JMap[String, _] = (Map(
      "kind"     -> ent.kind,
      "annotations" -> ent.annotations.asJava,
      "name"     -> ent.name,
      "path"     -> ent.path.asJava,
      "members"  -> ent.members.map(_.asJava()).asJava,
      "children"  -> ent.children.map(_.asJava()).asJava,
      "comment"  -> ent.comment.map(_.asJava).asJava,
      "superTypes" -> ent.superTypes,
      "hasVisibleMembers" -> ent.hasVisibleMembers
    ) ++ extras).asJava
  }

  implicit class JavaCaseClass(val ent: CaseClass) extends AnyVal {
    def asJava(extras: Map[String, _] = Map.empty): JMap[String, _] = (Map(
      "kind" -> ent.kind,
      "annotations" -> ent.annotations.asJava,
      "name" -> ent.name,
      "members" -> ent.members.map(_.asJava()).asJava,
      "modifiers" -> ent.modifiers.asJava,
      "path" -> ent.path.asJava,
      "typeParams" -> ent.typeParams.asJava,
      "superTypes" -> ent.superTypes.map(_.asJava).asJava,
      "comment" -> ent.comment.map(_.asJava).asJava,
      "isPrivate" -> ent.isPrivate,
      "isProtected" -> ent.isProtected,
      "hasVisibleMembers" -> ent.hasVisibleMembers
    ) ++ extras).asJava
  }

  implicit class JavaClass(val ent: Class) extends AnyVal {
    def asJava(extras: Map[String, _] = Map.empty): JMap[String, _] = (Map(
      "kind"       -> ent.kind,
      "annotations" -> ent.annotations.asJava,
      "name"       -> ent.name,
      "members"    -> ent.members.map(_.asJava()).asJava,
      "modifiers"  -> ent.modifiers.asJava,
      "path"       -> ent.path.asJava,
      "typeParams" -> ent.typeParams.asJava,
      "superTypes" -> ent.superTypes.map(_.asJava).asJava,
      "comment"    -> ent.comment.map(_.asJava).asJava,
      "isPrivate" -> ent.isPrivate,
      "isProtected" -> ent.isProtected,
      "hasVisibleMembers" -> ent.hasVisibleMembers
    ) ++ extras).asJava
  }

  implicit class JavaTrait(val ent: Trait) extends AnyVal {
    def asJava(extras: Map[String, _] = Map.empty): JMap[String, _] = (Map(
      "kind"       -> ent.kind,
      "annotations" -> ent.annotations.asJava,
      "name"       -> ent.name,
      "members"    -> ent.members.map(_.asJava()).asJava,
      "modifiers"  -> ent.modifiers.asJava,
      "path"       -> ent.path.asJava,
      "typeParams" -> ent.typeParams.asJava,
      "superTypes" -> ent.superTypes.map(_.asJava).asJava,
      "comment"    -> ent.comment.map(_.asJava).asJava,
      "isPrivate" -> ent.isPrivate,
      "isProtected" -> ent.isProtected,
      "hasVisibleMembers" -> ent.hasVisibleMembers
    ) ++ extras).asJava
  }

  implicit class JavaObject(val ent: Object) extends AnyVal {
    def asJava(extras: Map[String, _] = Map.empty): JMap[String, _] = (Map(
      "kind"       -> ent.kind,
      "annotations" -> ent.annotations.asJava,
      "name"       -> ent.name,
      "members"    -> ent.members.map(_.asJava()).asJava,
      "modifiers"  -> ent.modifiers.asJava,
      "path"       -> ent.path.asJava,
      "superTypes" -> ent.superTypes.map(_.asJava).asJava,
      "comment"    -> ent.comment.map(_.asJava).asJava,
      "isPrivate" -> ent.isPrivate,
      "isProtected" -> ent.isProtected,
      "hasVisibleMembers" -> ent.hasVisibleMembers
    ) ++ extras).asJava
  }

  implicit class JavaDef(val ent: Def) extends AnyVal {
    def asJava: JMap[String, _] = Map(
      "kind"                -> ent.kind,
      "annotations" -> ent.annotations.asJava,
      "name"                -> ent.name,
      "modifiers"           -> ent.modifiers.asJava,
      "path"                -> ent.path.asJava,
      "returnValue"         -> ent.returnValue.asJava,
      "typeParams"          -> ent.typeParams.asJava,
      "paramLists"          -> ent.paramLists.map(_.asJava).asJava,
      "comment"             -> ent.comment.map(_.asJava).asJava,
      "implicitlyAddedFrom" -> ent.implicitlyAddedFrom.map(_.asJava).asJava,
      "isPrivate" -> ent.isPrivate,
      "isProtected" -> ent.isProtected
    ).asJava
  }

  implicit class JavaVal(val ent: Val) extends AnyVal {
    def asJava: JMap[String, _] = Map(
      "kind" -> ent.kind,
      "annotations" -> ent.annotations.asJava,
      "name" -> ent.name,
      "modifiers" -> ent.modifiers.asJava,
      "path" -> ent.path.asJava,
      "returnValue" -> ent.returnValue.asJava,
      "comment" -> ent.comment.map(_.asJava).asJava,
      "implicitlyAddedFrom" -> ent.implicitlyAddedFrom.map(_.asJava).asJava,
      "isPrivate" -> ent.isPrivate,
      "isProtected" -> ent.isProtected
    ).asJava
  }

  implicit class JavaTypeAlias(val ent: TypeAlias) extends AnyVal {
    def asJava: JMap[String, _] = Map(
      "kind" -> ent.kind,
      "annotations" -> ent.annotations.asJava,
      "modifiers" -> ent.modifiers.asJava,
      "name" -> ent.name,
      "path" -> ent.path.asJava,
      "alias" -> ent.alias.map(_.asJava).asJava,
      "comment" -> ent.comment.map(_.asJava).asJava,
      "isPrivate" -> ent.isPrivate,
      "isProtected" -> ent.isProtected
    ).asJava
  }

  implicit class JavaParamList(val pl: ParamList) extends AnyVal {
    def asJava: JMap[String, _] = Map(
      "list"       -> pl.list.map(_.asJava).asJava,
      "isImplicit" -> pl.isImplicit
    ).asJava
  }

  implicit class JavaReference(val ref: Reference) extends AnyVal {
    def asJava: JMap[String, _] = ref match {
      case TypeReference(title, tpeLink, paramLinks) => Map(
        "kind"       -> "TypeReference",
        "title"      -> title,
        "tpeLink"    -> tpeLink.asJava,
        "paramLinks" -> paramLinks.map(_.asJava).asJava,
        "scala"      -> ref
      ).asJava

      case OrTypeReference(left, right) => Map(
        "kind"  -> "OrTypeReference",
        "left"  -> left.asJava,
        "right" -> right.asJava,
        "scala" -> ref
      ).asJava

      case AndTypeReference(left, right) => Map(
        "kind"  -> "AndTypeReference",
        "left"  -> left.asJava,
        "right" -> right.asJava,
        "scala" -> ref
      ).asJava

      case FunctionReference(args, returnValue) => Map(
        "kind" -> "FunctionReference",
        "args" -> args.map(_.asJava).asJava,
        "returnValue" -> returnValue.asJava,
        "scala" -> ref
      ).asJava

      case TupleReference(args) => Map(
        "kind" -> "TupleReference",
        "args" -> args.map(_.asJava).asJava,
        "scala" -> ref
      ).asJava

      case BoundsReference(low, high) => Map(
        "kind"  -> "BoundsReference",
        "low"   -> low.asJava,
        "hight" -> high.asJava,
        "scala" -> ref
      ).asJava

      case NamedReference(title, ref, isByName, isRepeated) => Map(
        "kind"       -> "NamedReference",
        "title"      -> title,
        "ref"        -> ref.asJava,
        "isByName"   -> isByName,
        "isRepeated" -> isRepeated,
        "scala"      -> ref
      ).asJava

      case ConstantReference(title) => Map(
        "kind"  -> "ConstantReference",
        "title" -> title,
        "scala" -> ref
      ).asJava

      case EmptyReference => ???
    }
  }

  implicit class JavaMaterializableLink(val link: MaterializableLink) extends AnyVal {
    def asJava: JMap[String, _] = link match {
      case UnsetLink(title, query) => Map(
        "kind"  -> "UnsetLink",
        "title" -> title,
        "query" -> query,
        "scala" -> link
      ).asJava

      case MaterializedLink(title, target) => Map(
        "kind"   -> "MaterializedLink",
        "title"  -> title,
        "target" -> target,
        "scala" -> link
      ).asJava

      case NoLink(title, target) => Map(
        "kind"   -> "NoLink",
        "title"  -> title,
        "target" -> target,
        "scala" -> link
      ).asJava
    }
  }

  implicit class JavaEntity(val ent: Entity) extends AnyVal {
    def asJava(extras: Map[String, _] = Map.empty): JMap[String, _] = parseEntity(ent, extras)
  }

  private def parseEntity(ent: Entity, extras: Map[String, _]): JMap[String, _] = ent match {
    case ent: Package   => ent.asJava(extras)
    case ent: CaseClass => ent.asJava(extras)
    case ent: Class     => ent.asJava(extras)
    case ent: Trait     => ent.asJava(extras)
    case ent: Object    => ent.asJava(extras)
    case ent: Def       => ent.asJava
    case ent: Val       => ent.asJava
    case ent: TypeAlias => ent.asJava
    case _              => Map.empty.asJava
  }

  implicit class JavaMap(val map: collection.Map[String, Package]) extends AnyVal {
    def toJavaList: LinkedList[AnyRef] = {
      map.toList
         .sortBy(_._1)
         .foldLeft(new LinkedList[AnyRef]()) { case (list, (_, pkg)) =>
           list.add(pkg.asJava())
           list
         }
    }
  }
}
