package dotty.dokka

import org.jetbrains.dokka.base.signatures._
import org.jetbrains.dokka.model._
import org.jetbrains.dokka.model.properties.{WithExtraProperties}
import org.jetbrains.dokka.pages._
import org.jetbrains.dokka.base.signatures.KotlinSignatureProvider
import org.jetbrains.dokka.base.transformers.pages.comments.CommentsToContentConverter
import org.jetbrains.dokka.utilities.DokkaLogger
import collection.JavaConverters._
import org.jetbrains.dokka.base.translators.documentables._
import org.jetbrains.dokka.model.properties.PropertyContainer
import java.util.function.Consumer
import kotlin.jvm.functions.Function2
import dotty.dokka.model.api.{Kind, _}


class ScalaSignatureProvider(contentConverter: CommentsToContentConverter)(using ctx: DocContext)
  extends SignatureProvider with ScalaSignatureUtils:
  private val styles = Set(TextStyle.Monospace).asInstanceOf[Set[Style]]
  private val contentBuilder = new ScalaPageContentBuilder(contentConverter, this)

  private def signatureContent(d: Documentable)(
    func: ScalaPageContentBuilder#ScalaDocumentableContentBuilder => ScalaPageContentBuilder#ScalaDocumentableContentBuilder
  ) =
    val styles = stylesIfDeprecated(d)
    contentBuilder.contentForDocumentable(d, kind = ContentKind.Symbol, styles = styles, buildBlock = func)


  case class ContentNodeBuilder(builder: ScalaPageContentBuilder#ScalaDocumentableContentBuilder) extends SignatureBuilder{
    def text(str: String): SignatureBuilder = ContentNodeBuilder(builder.text(str))
    def driLink(text: String, dri: DRI): SignatureBuilder = ContentNodeBuilder(builder.driLink(text, dri))
  }

  def signature(d: Member, s: Signature) = signatureContent(d){ builder =>
    val res = ContentNodeBuilder(builder).signature(s)
    res.asInstanceOf[ContentNodeBuilder].builder
  }

  override def signature(documentable: Member) =
    JList(signatureContent(documentable){ builder =>
      val withAnnotations = ContentNodeBuilder(builder).annotationsBlock(documentable)
      val res = ScalaSignatureProvider.rawSignature(documentable, withAnnotations)
      res.asInstanceOf[ContentNodeBuilder].builder
    })

  private def stylesIfDeprecated(m: Member): Set[Style] =
    if m.deprecated.isDefined then styles ++ Set(TextStyle.Strikethrough) else styles


object ScalaSignatureProvider:
  def rawSignature(documentable: Documentable, builder: SignatureBuilder): SignatureBuilder =
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
      case Kind.Given(Kind.Val, _, _) =>
        givenPropertySignature(documentable, builder)
      case cls: Kind.Class =>
        classSignature(documentable, cls, builder)
      case Kind.Object | Kind.Enum =>
        objectSignature(documentable, builder)
      case trt: Kind.Trait =>
        traitSignature(documentable, trt, builder)
      case Kind.Val | Kind.Var | Kind.Implicit(Kind.Val, _) =>
        fieldSignature(documentable, documentable.kind.name, builder)
      case tpe: Kind.Type =>
        typeSignature(tpe, documentable, builder)
      case Kind.Package =>
        builder.text("package").text(" ").name(documentable.name, documentable.dri)
      case Kind.Unknown =>
        ???

  private def enumEntrySignature(member: Member, cls: Kind.Class, bdr: SignatureBuilder): SignatureBuilder =
    val withPrefixes: SignatureBuilder = bdr
      .text("case ")
      .memberName(member.name, member.dri)
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
        val extendPart = builder.text(" extends ").signature(extendType)
        withTypes.foldLeft(extendPart)((bdr, tpe) => bdr.text(" with ").signature(tpe))

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
      .name(clazz.getName, clazz.getDri)
      .generics(cls.typeParams)
      .functionParameters(cls.argsLists)

    parentsSignature(clazz, selfSignature)

  private def objectSignature(clazz: Member, builder: SignatureBuilder): SignatureBuilder =
    val selfSignature = builder
      .modifiersAndVisibility(clazz, clazz.kind.name)
      .name(clazz.getName, clazz.getDri)

    parentsSignature(clazz, selfSignature)

  private def traitSignature(clazz: Member, cls: Kind.Trait, builder: SignatureBuilder): SignatureBuilder =
    val selfSignature = builder
      .modifiersAndVisibility(clazz, clazz.kind.name)
      .name(clazz.getName, clazz.getDri)
      .generics(cls.typeParams)
      .functionParameters(cls.argsLists)

    parentsSignature(clazz, selfSignature)

  private def extensionSignature(extension: Member, fun: Kind.Def, builder: SignatureBuilder): SignatureBuilder =
    val withSignature = builder
      .modifiersAndVisibility(extension, "def")
      .name(extension.getName, extension.getDri)
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
    .name(method.getName, method.getDri)
    .generics(cls.typeParams)
    .functionParameters(cls.argsLists)
    if !method.kind.isInstanceOf[Kind.Constructor] then
      bdr.text(": ").signature(method.signature)
    else bdr

  private def typeSignature(tpe: Kind.Type, typeDef: Member, builder: SignatureBuilder): SignatureBuilder =
    val bdr = builder
      .modifiersAndVisibility(typeDef, if tpe.opaque then "opaque type" else "type")
      .name(typeDef.getName, typeDef.getDri)
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
