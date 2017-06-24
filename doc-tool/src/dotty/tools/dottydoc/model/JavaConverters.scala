package dotty.tools.dottydoc
package model

import comment._
import references._

import java.util.LinkedList

object JavaConverters {
  import scala.collection.JavaConverters._
  import java.util.{ Map => JMap }

  implicit class OptStr(val opt: Option[String]) extends AnyVal {
    def asJava = opt.getOrElse(null)
  }

  implicit class OptMap(val opt: Option[JMap[String, _]]) extends AnyVal {
    def asJava = opt.getOrElse(Map.empty.asJava)
  }

  implicit class JavaComment(val cmt: Comment) extends AnyVal {
    def asJava: JMap[String, _] = Map(
      "body" -> cmt.body,
      "short" -> cmt.short,
      "authors" -> cmt.authors.asJava,
      "see" -> cmt.see.asJava,
      "result" -> cmt.result.asJava,
      "throws" -> cmt.throws.asJava,
      "valueParams" -> cmt.valueParams.asJava,
      "typeParams" -> cmt.typeParams.asJava,
      "version" -> cmt.version.asJava,
      "since" -> cmt.since.asJava,
      "todo" -> cmt.todo.asJava,
      "deprecated" -> cmt.deprecated.asJava,
      "note" -> cmt.note.asJava,
      "example" -> cmt.example.asJava,
      "constructor" -> cmt.constructor.asJava,
      "group" -> cmt.group.asJava,
      "groupDesc" -> cmt.groupDesc.asJava,
      "groupNames" -> cmt.groupNames.asJava,
      "groupPrio" -> cmt.groupPrio.asJava,
      "hideImplicitConversions" -> cmt.hideImplicitConversions.asJava
    ).asJava
  }

  implicit class JavaParamList(val pl: ParamList) extends AnyVal {
    def asJava: JMap[String, _] = Map(
      "list" -> pl.list.map(_.asJava).asJava,
      "isImplicit" -> pl.isImplicit
    ).asJava
  }

  implicit class JavaReference(val ref: Reference) extends AnyVal {
    def asJava: JMap[String, _] = ref match {
      case TypeReference(title, tpeLink, paramLinks) => Map(
        "kind" -> "TypeReference",
        "title" -> title,
        "tpeLink" -> tpeLink.asJava,
        "paramLinks" -> paramLinks.map(_.asJava).asJava,
        "scala" -> ref
      ).asJava

      case OrTypeReference(left, right) => Map(
        "kind" -> "OrTypeReference",
        "left" -> left.asJava,
        "right" -> right.asJava,
        "scala" -> ref
      ).asJava

      case AndTypeReference(left, right) => Map(
        "kind" -> "AndTypeReference",
        "left" -> left.asJava,
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
        "kind" -> "BoundsReference",
        "low" -> low.asJava,
        "hight" -> high.asJava,
        "scala" -> ref
      ).asJava

      case NamedReference(title, ref, isByName, isRepeated) => Map(
        "kind" -> "NamedReference",
        "title" -> title,
        "ref" -> ref.asJava,
        "isByName" -> isByName,
        "isRepeated" -> isRepeated,
        "scala" -> ref
      ).asJava

      case ConstantReference(title) => Map(
        "kind" -> "ConstantReference",
        "title" -> title,
        "scala" -> ref
      ).asJava

      case EmptyReference =>
        throw new IllegalStateException("Empty reference discovered while converting to Java Map")
    }
  }

  implicit class JavaMaterializableLink(val link: MaterializableLink) extends AnyVal {
    def asJava: JMap[String, _] = link match {
      case UnsetLink(title, query) => Map(
        "kind" -> "UnsetLink",
        "title" -> title,
        "query" -> query,
        "scala" -> link
      ).asJava

      case MaterializedLink(title, target) => Map(
        "kind" -> "MaterializedLink",
        "title" -> title,
        "target" -> target,
        "scala" -> link
      ).asJava

      case NoLink(title, target) => Map(
        "kind" -> "NoLink",
        "title" -> title,
        "target" -> target,
        "scala" -> link
      ).asJava
    }
  }

  implicit class JavaEntity(val ent: Entity) extends AnyVal {
    def asJava: JMap[String, _] = parseEntity(ent)
  }

  private def parseEntity(ent: Entity): JMap[String, _] = {
    val entity = Map(
      "kind" -> ent.kind,
      "annotations" -> ent.annotations.asJava,
      "name" -> ent.name,
      "path" -> ent.path.asJava,
      "children" -> ent.children.map(_.asJava).asJava,
      "comment" -> ent.comment.map(_.asJava).asJava,
      "signature" -> ent.signature
    )
    val members = ent match {
      case ent: Members => Map(
        "members" -> ent.members.map(_.asJava).asJava,
        "hasVisibleMembers" -> ent.hasVisibleMembers
      )
      case _ => Map.empty
    }
    val superTypes = ent match {
      case ent: SuperTypes => Map(
        "superTypes" -> ent.superTypes.map(_.asJava).asJava
      )
      case _ => Map.empty
    }
    val modifiers = ent match {
      case ent: Modifiers => Map(
        "modifiers" -> ent.modifiers.asJava,
        "isPrivate" -> ent.isPrivate,
        "isProtected" -> ent.isProtected
      )
      case _ => Map.empty
    }
    val typeParams = ent match {
      case ent: TypeParams => Map(
        "typeParams" -> ent.typeParams.asJava
      )
      case _ => Map.empty
    }
    val constructors = ent match {
      case ent: Constructors => Map(
        "constructors" -> ent.constructors.map(_.map(_.asJava).asJava).asJava
      )
      case _ => Map.empty
    }
    val companion = ent match {
      case ent: Companion => Map(
        "hasCompanion" -> ent.hasCompanion,
        "companionPath" -> ent.companionPath.asJava
      )
      case _ => Map.empty
    }
    val returnValue = ent match {
      case ent: ReturnValue => Map(
        "returnValue" -> ent.returnValue.asJava
      )
      case _ => Map.empty
    }
    val implicitlyAddedEntity = ent match {
      case ent: ImplicitlyAddedEntity => Map(
        "implicitlyAddedFrom" -> ent.implicitlyAddedFrom.map(_.asJava).asJava
      )
      case _ => Map.empty
    }
    val typeAlias : Map[String, _] = ent match {
      case ent: TypeAlias => Map(
        "alias" -> ent.alias.map(_.asJava).asJava
      )
      case _ => Map.empty
    }
    val trt = ent match {
      case ent: Trait => Map(
        "traitParams" -> ent.traitParams.map(_.asJava).asJava
      )
      case _ => Map.empty
    }
    val df = ent match {
      case ent: Def => Map(
        "paramLists" -> ent.paramLists.map(_.asJava).asJava
      )
      case _ => Map.empty
    }

    {
      entity ++
      members ++
      superTypes ++
      modifiers ++
      typeParams ++
      constructors ++
      companion ++
      returnValue ++
      implicitlyAddedEntity ++
      typeAlias ++
      trt ++
      df
    }.asJava
  }

  implicit class JavaMap(val map: collection.Map[String, Package]) extends AnyVal {
    def toJavaList: LinkedList[AnyRef] =
      convertToList(map.mapValues(_.asJava))

    def flattened: LinkedList[AnyRef] =
      convertToList(map.mapValues(flattenEntity))

    private[this] def convertToList(ms: collection.Map[String, AnyRef]): LinkedList[AnyRef] =
      ms.toList.sortBy(_._1)
        .foldLeft(new LinkedList[AnyRef]()) { case (list, (_, value)) =>
          list.add(value); list
        }

    private[this] def flattenEntity(e: Entity): JMap[String, _] = {
      def entity(e: Entity) =
        Map("name" -> e.name, "path" -> e.path.asJava, "kind" -> e.kind)

      def members(e: Entity with Members) =
        Map("members" -> e.members.map(flattenEntity).asJava)

      def companion(e: Companion) = Map(
        "hasCompanion" -> e.hasCompanion,
        "companionPath" -> e.companionPath.asJava
      )

      def typeParams(e: TypeParams) =
        Map("typeParams" -> e.typeParams.asJava)

      def paramLists(e: Def) = Map(
        "paramLists" -> {
          e.paramLists.map { paramList =>
            Map(
              "isImplicit" -> paramList.isImplicit,
              "list" -> paramList.list.map(_.showReference).asJava
            ).asJava
          }
          .asJava
        }
      )

      def returnValue(e: ReturnValue) =
        Map("returnValue" -> e.returnValue.showReference)

      entity(e) ++ (e match {
        case e: Package   => members(e)
        case e: Class     => members(e) ++ companion(e)
        case e: CaseClass => members(e) ++ companion(e)
        case e: Trait     => members(e) ++ companion(e)
        case e: Object    => members(e) ++ companion(e)
        case e: Def       => typeParams(e) ++ paramLists(e) ++ returnValue(e)
        case e: TypeAlias => Map.empty
        case e: Val       => Map.empty
      })
    }.asJava
  }
}
