package dotty.tools.scaladoc

import dotty.tools.scaladoc.util._

object Inkuire {

  var db = InkuireDb(Seq.empty, Map.empty, Seq.empty, Map.empty)
  var implicitConversions: Seq[(Option[TypeLike], Type)] = Seq.empty

  def beforeSave(): Unit = {
    db = db.copy(
      functions = functions.sortBy(_.hashCode),
      types = db.types.toSeq.sortBy(_._1.uuid).toMap,
      implicitConversions = implicitConversions.collect {
        case (Some(from), to) => from -> to
      }.sortBy(_._1.hashCode)
    )
  }

  def generateInkuireConfig(externalMappings: Seq[String]): String = {
    val paths =
      List("../inkuire-db.json").map(jsonString)
        // TODO: #13243
        // ++ externalMappings.map(_ + "../inkuire-db.json")).map(jsonString)
    jsonObject(("inkuirePaths", jsonList(paths))).toString
  }

  def curry(e: Signature): Signature = {
    e.result.typ match
      case t: Type if t.name.name == s"Function${t.params.size-1}" =>
        curry(
          e.copy(
            arguments = e.arguments ++ t.params.init.map(_.typ).map(Contravariance(_)),
            result = Covariance(t.params.last.typ)
          )
        )
      case _ => e
  }

  def functions: Seq[ExternalSignature] = Inkuire.db.functions.flatMap { func =>
    val fromConversions = Inkuire.implicitConversions.filter { ic =>
      func.signature.receiver.nonEmpty && matchingICTypes(ic._2, func.signature.receiver.get.typ)
    }.map {
      case (Some(from), to) =>
        func.copy(
          signature = func.signature.copy(
            receiver = func.signature.receiver.map { rcvr =>
              Contravariance(
                newReceiver(rcvr.typ, to, from)
              )
            }
          )
        )
      case (None, to) =>
        func.copy(
          signature = func.signature.copy(
            receiver = None
          )
        )
    }
    Seq(func) ++ fromConversions
  }
  .distinct

  def matchingICTypes(a: TypeLike, b: TypeLike): Boolean = (a, b) match {
    case (a: Type, b: Type) if a.params.size == 0 && b.params.size == 0 && a.itid == b.itid => true
    case (a: Type, b: Type) if a.params.size == 1 && b.params.size == 1 && a.itid == b.itid =>
      a.params.head.typ.isInstanceOf[Type] && a.params.head.typ.asInstanceOf[Type].isVariable &&
        b.params.head.typ.isInstanceOf[Type] && b.params.head.typ.asInstanceOf[Type].isVariable
    case _ => false
  }

  def newReceiver(old: TypeLike, to: TypeLike, from: TypeLike): TypeLike = (old, to) match {
    case (a: Type, b: Type) if a.params.size == 0 && b.params.size == 0 && a.itid == b.itid => from
    case (a: Type, b: Type) if a.params.size == 1 && b.params.size == 1 && a.itid == b.itid
      && a.params.head.typ.isInstanceOf[Type] && a.params.head.typ.asInstanceOf[Type].isVariable &&
        b.params.head.typ.isInstanceOf[Type] && b.params.head.typ.asInstanceOf[Type].isVariable =>
      if from.isInstanceOf[Type] && from.asInstanceOf[Type].params.size == 1 && from.asInstanceOf[Type].params.head.typ.isInstanceOf[Type] && from.asInstanceOf[Type].params.head.typ.asInstanceOf[Type].isVariable then
        from.asInstanceOf[Type].copy(
          params = Seq(Contravariance(a.params.head.typ.asInstanceOf[Type]))
        )
      else if from.isInstanceOf[Type] && from.asInstanceOf[Type].isVariable then
        a.params.head.typ.asInstanceOf[Type]
      else
        from
    case _ => old
  }

  case class InkuireDb(
    functions:           Seq[ExternalSignature],
    types:               Map[ITID, (Type, Seq[Type])],
    implicitConversions: Seq[(TypeLike, Type)],
    typeAliases:         Map[ITID, TypeLike]
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
    def apply(receiver: Option[TypeLike], arguments: Seq[TypeLike], result: TypeLike, context: SignatureContext): Signature =
      Signature(receiver.map(Contravariance(_)), arguments.map(Contravariance(_)), Covariance(result), context)
  }

  case class ExternalSignature(
    signature:    Signature,
    name:         String,
    packageName:  String,
    uri:          String,
    isLocationExternal: Boolean,
    entryType:    String
  )

  sealed trait TypeLike

  case class Type(
    name:             TypeName,
    params:           Seq[Variance] = Seq.empty,
    nullable:         Boolean = false,
    itid:             Option[ITID] = None,
    isVariable:       Boolean = false,
    isStarProjection: Boolean = false,
    isUnresolved:     Boolean = false
  ) extends TypeLike

  case class AndType(left: TypeLike, right: TypeLike) extends TypeLike
  case class OrType(left: TypeLike, right: TypeLike) extends TypeLike

  case class TypeLambda(args: Seq[Type], result: TypeLike) extends TypeLike

  object TypeLambda {
    def argument(name: String): Type =
      val uuid = s"external-type-lambda-arg-$name"
      Inkuire.Type(
        name = Inkuire.TypeName(name),
        itid = Some(Inkuire.ITID(uuid, isParsed = false)),
        isVariable = true
      )
  }

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
    constraints: Map[String, Seq[TypeLike]]
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
    val typ: TypeLike
  }

  case class Covariance(typ: TypeLike) extends Variance

  case class Contravariance(typ: TypeLike) extends Variance

  case class Invariance(typ: TypeLike) extends Variance

  case class UnresolvedVariance(typ: TypeLike) extends Variance

  object EngineModelSerializers {
    def serialize(db: InkuireDb): JSON = {
      jsonObject(
        ("types", serialize(db.types)),
        ("functions", jsonList(db.functions.map(serialize))),
        ("implicitConversions", jsonList(db.implicitConversions.map(serializeConversion))),
        ("typeAliases", serializeTypeAliases(db.typeAliases))
      )
    }

    private def serializeConversion(conversion: (TypeLike, Type)): JSON = {
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

    private def serializeTypeAliases(types: Map[ITID, TypeLike]): JSON = {
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

    private def serialize(t: TypeLike): JSON = t match {
      case t: Type =>
        jsonObject(
          ("name", serialize(t.name)),
          ("params", jsonList(t.params.map(serialize))),
          ("nullable", serialize(t.nullable)),
          ("itid", serialize(t.itid.get)),
          ("isVariable", serialize(t.isVariable)),
          ("isStarProjection", serialize(t.isStarProjection)),
          ("isUnresolved", serialize(t.isUnresolved)),
          ("typelikekind", serialize("type"))
        )
      case t: OrType =>
        jsonObject(
          ("left", serialize(t.left)),
          ("right", serialize(t.right)),
          ("typelikekind", serialize("ortype"))
        )
      case t: AndType =>
        jsonObject(
          ("left", serialize(t.left)),
          ("right", serialize(t.right)),
          ("typelikekind", serialize("andtype"))
        )
      case t: TypeLambda =>
        jsonObject(
          ("args", jsonList(t.args.map(serialize))),
          ("result", serialize(t.result)),
          ("typelikekind", serialize("typelambda"))
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
        ("uri", serialize((if e.isLocationExternal then "e" else "i") + e.uri)),
        ("entryType", serialize(e.entryType))
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

    private def serializeConstraints(constraints: Map[String, Seq[TypeLike]]): JSON = {
      jsonObject((
        constraints.toList.map {
          case (name, vs) =>
            (name, jsonList(vs.map(serialize)))
        }
      )*)
    }
  }

}
