package dotty.tools.scaladoc

import dotty.tools.scaladoc.util._
import scala.collection.mutable.{ Map => MMap}

object Inkuire {

  var db = InkuireDb(Seq.empty, Map.empty, Seq.empty)

  def beforeSave(): Unit = {
    db = db.copy(
      functions = db.functions.sortBy(_.hashCode),
      types = db.types.toSeq.sortBy(_._1.uuid).toMap,
      implicitConversions = db.implicitConversions.sortBy(_._1.uuid)
    )
  }

  def generateInkuireConfig(externalMappings: Seq[String]): String = {
    val paths = ("../inkuire-db.json" +: externalMappings.map(_ + "../inkuire-db.json")).map(jsonString)
    jsonObject(("inkuirePaths", jsonList(paths))).toString
  }

  case class InkuireDb(
    functions:           Seq[ExternalSignature],
    types:               Map[ITID, (Type, Seq[Type])],
    implicitConversions: Seq[(ITID, Type)]
  )

  case class ITID(uuid: String, isParsed: Boolean)

  case class Signature(
    receiver:  Option[Contravariance],
    arguments: Seq[Contravariance],
    result:    Covariance,
    context:   SignatureContext
  ) {
    def typesWithVariances: Seq[Variance] = receiver.toSeq ++ arguments ++ Seq(result)
  }

  object Signature {
    def apply(receiver: Option[Type], arguments: Seq[Type], result: Type, context: SignatureContext): Signature =
      Signature(receiver.map(Contravariance(_)), arguments.map(Contravariance(_)), Covariance(result), context)
  }

  case class ExternalSignature(
    signature:    Signature,
    name:         String,
    packageName:  String,
    uri:      String
  )

  case class Type(
    name: TypeName,
    params: Seq[Variance] = Seq.empty,
    nullable: Boolean = false,
    itid: Option[ITID] = None,
    isVariable: Boolean = false,
    isStarProjection: Boolean = false,
    isUnresolved: Boolean = false
  )

  object Type {
    def unresolved: Type =
      Type(
        name = TypeName("<unresolved>"),
        itid = Some(
          ITID(
            uuid = "<unresolved>",
            isParsed = false
          )
        )
      )

    def StarProjection: Type =
      Type(
        name = TypeName("_"),
        itid = Some(ITID("_", isParsed = false)),
        isStarProjection = true
      )
  }

  case class TypeName(name: String) {
    override def hashCode: Int = name.toLowerCase.hashCode

    override def equals(obj: Any): Boolean = {
      obj match {
        case o: TypeName => this.name.toLowerCase == o.name.toLowerCase
        case _ => false
      }
    }

    override def toString: String = name
  }

  case class SignatureContext(
    vars:        Set[String],
    constraints: Map[String, Seq[Type]]
  ) {
    override def hashCode: Int = vars.size.hashCode

    override def equals(obj: Any): Boolean =
      obj match {
        case other: SignatureContext if this.vars.size == other.vars.size => true
        case _ => false
      }
  }

  object SignatureContext {
    def empty: SignatureContext = SignatureContext(Set.empty, Map.empty)
  }

  sealed abstract class Variance {
    val typ: Type
  }

  case class Covariance(typ: Type) extends Variance

  case class Contravariance(typ: Type) extends Variance

  case class Invariance(typ: Type) extends Variance

  case class UnresolvedVariance(typ: Type) extends Variance

  object EngineModelSerializers {
    def serialize(db: InkuireDb): JSON = {
      jsonObject(
        ("types", serialize(db.types)),
        ("functions", jsonList(db.functions.map(serialize))),
        ("implicitConversions", jsonList(db.implicitConversions.map(serializeConversion)))
      )
    }

    private def serializeConversion(conversion: (ITID, Type)): JSON = {
      jsonList(
        Seq(
          serialize(conversion._1),
          serialize(conversion._2)
        )
      )
    }

    private def serialize(types: Map[ITID, (Type, Seq[Type])]): JSON = {
      jsonObject((
        types.toList.map {
          case (itid, v) =>
            (serializeAsKey(itid), serialize(v))
        }
      )*)
    }

    private def serializeAsKey(itid: ITID): String = {
      s"""${itid.isParsed}=${itid.uuid}"""
    }

    private def serialize(v: (Type, Seq[Type])): JSON = {
      jsonList(
        Seq(
          serialize(v._1),
          jsonList(v._2.map(serialize))
        )
      )
    }

    private def serialize(t: Type): JSON = {
      jsonObject(
        ("name", serialize(t.name)),
        ("params", jsonList(t.params.map(serialize))),
        ("nullable", serialize(t.nullable)),
        ("itid", serialize(t.itid.get)),
        ("isVariable", serialize(t.isVariable)),
        ("isStarProjection", serialize(t.isStarProjection)),
        ("isUnresolved", serialize(t.isUnresolved))
      )
    }

    private def serialize(b: Boolean): JSON = {
      if b then rawJSON("true") else rawJSON("false")
    }

    private def serialize(itid: ITID): JSON = {
      jsonObject(
        ("uuid", serialize(itid.uuid)),
        ("isParsed", serialize(itid.isParsed))
      )
    }

    private def serialize(s: TypeName): JSON = {
      jsonObject(
        ("name", serialize(s.name))
      )
    }

    private def serialize(v: Variance): JSON = v match {
      case _: Invariance =>
        jsonObject(
          ("typ", serialize(v.typ)),
          ("variancekind", serialize("invariance"))
        )
      case _: Covariance =>
        jsonObject(
          ("typ", serialize(v.typ)),
          ("variancekind", serialize("covariance"))
        )
      case _: Contravariance =>
        jsonObject(
          ("typ", serialize(v.typ)),
          ("variancekind", serialize("contravariance"))
        )
      case _: UnresolvedVariance =>
        jsonObject(
          ("typ", serialize(v.typ)),
          ("variancekind", serialize("unresolved"))
        )
    }

    private def serialize(e: ExternalSignature): JSON = {
      jsonObject(
        ("signature", serialize(e.signature)),
        ("name", serialize(e.name)),
        ("packageName", serialize(e.packageName)),
        ("uri", serialize(e.uri))
      )
    }

    private def serialize(s: String): JSON = {
      jsonString(s)
    }

    private def serialize(s: Signature): JSON = {
      jsonObject(
        ("receiver", serialize(s.receiver)),
        ("arguments", jsonList(s.arguments.map(serialize))),
        ("result", serialize(s.result)),
        ("context", serialize(s.context))
      )
    }

    private def serialize(o: Option[Contravariance]): JSON = {
      o.fold(rawJSON("null")) { v =>
        serialize(v)
      }
    }

    private def serialize(c: SignatureContext): JSON = {
      jsonObject(
        ("vars", jsonList(c.vars.toSeq.map(serialize))),
        ("constraints", serializeConstraints(c.constraints))
      )
    }

    private def serializeConstraints(constraints: Map[String, Seq[Type]]): JSON = {
      jsonObject((
        constraints.toList.map {
          case (name, vs) =>
            (name, jsonList(vs.map(serialize)))
        }
      )*)
    }
  }

}
