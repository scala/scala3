package dotty.tools.scaladoc
package translators

import scala.util.chaining._

class ScalaSignatureProvider:
  val builder = SignatureBuilder()
  given Conversion[SignatureBuilder, Signature] = bdr => bdr.content
  def rawSignature(documentable: Member)(allowCC: Boolean /*capture checking enabled?*/)(kind: Kind = documentable.kind): MemberSignature =
    kind match
      case Kind.Extension(_, m) =>
        extensionSignature(documentable, m)
      case Kind.Exported(d) =>
        rawSignature(documentable)(allowCC)(d)
      case d: Kind.Def =>
        methodSignature(documentable, d)
      case Kind.Constructor(d) =>
        methodSignature(documentable, d)
      case Kind.Implicit(d: Kind.Def, _) =>
        methodSignature(documentable, d)
      case Kind.EnumCase(cls: Kind.Class) =>
        enumEntrySignature(documentable, cls)
      case Kind.EnumCase(_) =>
        enumPropertySignature(documentable)
      case Kind.Given(cls: Kind.Class, _, _) =>
        givenClassSignature(documentable, cls)
      case Kind.Given(d: Kind.Def, _, _) =>
        givenMethodSignature(documentable, d)
      case Kind.Given(Kind.Val, _, _) =>
        givenValSignature(documentable)
      case cls: Kind.Class =>
        classSignature(documentable, cls)
      case enm: Kind.Enum =>
        enumSignature(documentable, enm)
      case Kind.Object =>
        objectSignature(documentable)
      case trt: Kind.Trait =>
        traitSignature(documentable, trt)
      case Kind.Val | Kind.Var | Kind.Implicit(Kind.Val, _) =>
        fieldSignature(documentable, kind)
      case tpe: Kind.Type =>
        typeSignature(tpe, documentable)(allowCC)
      case Kind.Package =>
        MemberSignature(
          Nil,
          builder.keyword("package "),
          builder.name(documentable.name, documentable.dri),
          Nil
        )
      case Kind.RootPackage =>
        ???
      case Kind.Unknown =>
        ???

  private def classLikeSignature(member: Member, kind: Kind & Classlike)(showKind: Kind = kind): MemberSignature =
    MemberSignature(
      builder.modifiersAndVisibility(member),
      builder.kind(showKind),
      builder.name(member.name, member.dri),
      builder
        .typeParamList(kind.typeParams)
        .functionTermParameters(kind.argsLists)
        .parentsSignature(member)
    )


  private def enumEntrySignature(member: Member, cls: Kind.Class): MemberSignature =
    classLikeSignature(member, cls)()

  private def enumPropertySignature(entry: Member): MemberSignature =
    val modifiedType = entry.signature.map {
      case Keyword(" & ") => Keyword(" with ")
      case o => o
    }

    MemberSignature(
      builder.modifiersAndVisibility(entry),
      List(),
      builder.name(entry.name, entry.dri),
      builder.keyword(" extends ").signature(modifiedType)
    )

  private def givenClassSignature(member: Member, cls: Kind.Class): MemberSignature =
    val initialSignature = classLikeSignature(member, cls)(member.kind)

    member.kind match
      case Kind.Given(_, Some(instance), _) => initialSignature
        .copy(suffix = initialSignature.suffix ++ builder.plain(": ").signature(instance))
      case _ => initialSignature

  private def classSignature(clazz: Member, cls: Kind.Class): MemberSignature =
    classLikeSignature(clazz, cls)()

  private def objectSignature(clazz: Member): MemberSignature =
    classLikeSignature(clazz, Kind.Object)()

  private def traitSignature(clazz: Member, cls: Kind.Trait): MemberSignature =
    classLikeSignature(clazz, cls)()

  private def enumSignature(clazz: Member, cls: Kind.Enum): MemberSignature =
    classLikeSignature(clazz, cls)()

  private def methodLikeSignature(method: Member, kind: Kind.Def, instance: Option[Signature] = None)(showKind: Kind = kind): MemberSignature =
    MemberSignature(
      builder.modifiersAndVisibility(method),
      builder.kind(showKind),
      builder.name(method.name, method.dri),
      builder
        .functionParameters(kind.paramLists)
        .pipe { builder =>
          instance.fold(builder)(i => builder.plain(": ").signature(i))
        }
    )

  private def methodSignature(method: Member, cls: Kind.Def): MemberSignature =
    method.kind match {
      case _: Kind.Constructor => methodLikeSignature(method, cls, None)()
      case _ => methodLikeSignature(method, cls, Some(method.signature))()
    }

  private def extensionSignature(extension: Member, fun: Kind.Def): MemberSignature =
    methodLikeSignature(extension, fun, Some(extension.signature))()

  private def givenMethodSignature(method: Member, body: Kind.Def): MemberSignature = method.kind match
    case Kind.Given(_, iOpt @ Some(instance), _) =>
      methodLikeSignature(method, body, iOpt)(method.kind)
    case _ =>
      methodLikeSignature(method, body)(method.kind)

  private def fieldLikeSignature(member: Member, kind: Kind, instance: Option[Signature] = None): MemberSignature =
    MemberSignature(
      builder.modifiersAndVisibility(member),
      builder.kind(kind),
      builder.name(member.name, member.dri),
      instance.fold(builder)(i => builder.plain(": ").signature(i))
    )


  private def fieldSignature(member: Member, kind: Kind): MemberSignature =
    fieldLikeSignature(member, kind, Some(member.signature))

  private def givenValSignature(field: Member): MemberSignature = field.kind match
    case Kind.Given(_, iOpt @ Some(instance), _) =>
      fieldLikeSignature(field, field.kind, iOpt)
    case _ =>
      fieldLikeSignature(field, field.kind, None)

  private def typeSignature(tpe: Kind.Type, typeDef: Member)(allowCC: Boolean = false): MemberSignature =
    MemberSignature(
      builder.modifiersAndVisibility(typeDef),
      builder.kind(tpe),
      builder.name(typeDef.name, typeDef.dri, isCaptureVar = allowCC && tpe.isCaptureVar),
      builder.typeParamList(tpe.typeParams).pipe { bdr =>
        if (!tpe.opaque) {
          (if tpe.concreate then bdr.plain(" = ") else bdr)
            .signature(typeDef.signature)
        } else bdr
      }
    )
