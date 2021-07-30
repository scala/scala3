package dotty.tools.scaladoc
package translators

object ScalaSignatureProvider:
  def rawSignature(documentable: Member, builder: SignatureBuilder): SignatureBuilder =
    documentable.kind match
      case Kind.Extension(_, m) =>
        extensionSignature(documentable, m, builder)
      case Kind.Exported(d) =>
         methodSignature(documentable, d, builder)
      case d: Kind.Def =>
        methodSignature(documentable, d, builder)
      case Kind.Constructor(d) =>
        methodSignature(documentable, d, builder)
      case Kind.Implicit(d: Kind.Def, _) =>
        methodSignature(documentable, d, builder)
      case Kind.EnumCase(cls: Kind.Class) =>
        enumEntrySignature(documentable, cls, builder)
      case Kind.EnumCase(_) =>
       enumPropertySignature(documentable, builder)
      case Kind.Given(cls: Kind.Class, _, _) =>
        givenClassSignature(documentable, cls, builder)
      case Kind.Given(d: Kind.Def, _, _) =>
        givenMethodSignature(documentable, d, builder)
      case cls: Kind.Class =>
        classSignature(documentable, cls, builder)
      case enm: Kind.Enum =>
        enumSignature(documentable, enm, builder)
      case Kind.Object =>
        objectSignature(documentable, builder)
      case trt: Kind.Trait =>
        traitSignature(documentable, trt, builder)
      case Kind.Val | Kind.Var | Kind.Implicit(Kind.Val, _) =>
        fieldSignature(documentable, documentable.kind.name, builder)
      case tpe: Kind.Type =>
        typeSignature(tpe, documentable, builder)
      case Kind.Package =>
        builder.text("package ").name(documentable.name, documentable.dri)
      case Kind.RootPackage =>
        builder
      case Kind.Unknown =>
        ???

  private def enumEntrySignature(member: Member, cls: Kind.Class, bdr: SignatureBuilder): SignatureBuilder =
    val withPrefixes: SignatureBuilder = bdr
      .text("case ")
      .name(member.name, member.dri)
      .generics(cls.typeParams)

    val withParameters = withPrefixes.functionParameters(cls.argsLists)
    parentsSignature(member, withParameters)

  private def enumPropertySignature(entry: Member, builder: SignatureBuilder): SignatureBuilder =
    val modifiedType = entry.signature.map {
      case " & " => " with "
      case o => o
    }

    builder
      .text("case ")
      .name(entry.name, entry.dri)
      .text(" extends ")
      .signature(modifiedType)

  private def parentsSignature(member: Member, builder: SignatureBuilder): SignatureBuilder =
    member.directParents match
      case Nil => builder
      case extendType :: withTypes =>
        val extendPart = builder.text(" extends ").signature(extendType.signature)
        withTypes.foldLeft(extendPart)((bdr, tpe) => bdr.text(" with ").signature(tpe.signature))

  private def givenClassSignature(member: Member, cls: Kind.Class, builder: SignatureBuilder): SignatureBuilder =
    val prefixes = builder
      .modifiersAndVisibility(member, "given")
      .name(member.name, member.dri)
      .generics(cls.typeParams)
      .functionParameters(cls.argsLists)

    member.kind match
      case Kind.Given(_, Some(instance), _) => prefixes
        .text(": ")
        .signature(instance)
      case _ => prefixes

  private def classSignature(clazz: Member, cls: Kind.Class, builder: SignatureBuilder): SignatureBuilder =
    val selfSignature = builder
      .modifiersAndVisibility(clazz, clazz.kind.name)
      .name(clazz.name, clazz.dri)
      .generics(cls.typeParams)
      .functionParameters(cls.argsLists)

    parentsSignature(clazz, selfSignature)

  private def objectSignature(clazz: Member, builder: SignatureBuilder): SignatureBuilder =
    val selfSignature = builder
      .modifiersAndVisibility(clazz, clazz.kind.name)
      .name(clazz.name, clazz.dri)

    parentsSignature(clazz, selfSignature)

  private def traitSignature(clazz: Member, cls: Kind.Trait, builder: SignatureBuilder): SignatureBuilder =
    val selfSignature = builder
      .modifiersAndVisibility(clazz, clazz.kind.name)
      .name(clazz.name, clazz.dri)
      .generics(cls.typeParams)
      .functionParameters(cls.argsLists)

    parentsSignature(clazz, selfSignature)

  private def enumSignature(clazz: Member, cls: Kind.Enum, builder: SignatureBuilder): SignatureBuilder =
    val selfSignature = builder
      .modifiersAndVisibility(clazz, clazz.kind.name)
      .name(clazz.name, clazz.dri)
      .generics(cls.typeParams)
      .functionParameters(cls.argsLists)

    parentsSignature(clazz, selfSignature)

  private def extensionSignature(extension: Member, fun: Kind.Def, builder: SignatureBuilder): SignatureBuilder =
    val withSignature = builder
      .modifiersAndVisibility(extension, "def")
      .name(extension.name, extension.dri)
      .generics(fun.typeParams)
      .functionParameters(fun.argsLists)

      withSignature.text(":").text(" ").signature(extension.signature)

  private def givenMethodSignature(method: Member, body: Kind.Def, builder: SignatureBuilder): SignatureBuilder = method.kind match
    case Kind.Given(_, Some(instance), _) =>
      builder.text("given ")
        .name(method.name, method.dri)
        .text(": ")
        .signature(instance)
    case _ =>
      builder.text("given ").name(method.name, method.dri)

  private def methodSignature(method: Member, cls: Kind.Def, builder: SignatureBuilder): SignatureBuilder =
    val bdr = builder
    .modifiersAndVisibility(method, "def")
    .name(method.name, method.dri)
    .generics(cls.typeParams)
    .functionParameters(cls.argsLists)
    if !method.kind.isInstanceOf[Kind.Constructor] then
      bdr.text(": ").signature(method.signature)
    else bdr

  private def typeSignature(tpe: Kind.Type, typeDef: Member, builder: SignatureBuilder): SignatureBuilder =
    val bdr = builder
      .modifiersAndVisibility(typeDef, if tpe.opaque then "opaque type" else "type")
      .name(typeDef.name, typeDef.dri)
      .generics(tpe.typeParams)
    if(!tpe.opaque){
      (if tpe.concreate then bdr.text(" = ") else bdr)
        .signature(typeDef.signature)
    } else bdr


  private def givenPropertySignature(property: Member, builder: SignatureBuilder): SignatureBuilder =
    val bdr = builder
      .text("given ")
      .name(property.name, property.dri)

    property.kind match
      case Kind.Given(_, Some(instance), _) =>
         bdr.text(" as ").signature(instance)
      case _ => bdr

  private def fieldSignature(member: Member, kind: String, builder: SignatureBuilder): SignatureBuilder =
    builder
      .modifiersAndVisibility(member, kind)
      .name(member.name, member.dri)
      .text(":")
      .text(" ")
      .signature(member.signature)
