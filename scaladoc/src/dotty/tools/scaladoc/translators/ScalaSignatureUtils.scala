package dotty.tools.scaladoc
package translators

case class SignatureBuilder(content: Signature = Nil) extends ScalaSignatureUtils:
  def plain(str: String): SignatureBuilder = copy(content = content :+ Plain(str))
  def name(str: String, dri: DRI, isCaptureVar: Boolean = false/*under CC*/): SignatureBuilder =
    val suffix = if isCaptureVar then List(Keyword("^")) else Nil
    copy(content = content ++ (Name(str, dri) :: suffix))
  def tpe(text: String, dri: Option[DRI], isCaptureVar: Boolean = false/*under CC*/): SignatureBuilder =
    val suffix = if isCaptureVar then List(Keyword("^")) else Nil
    copy(content = content ++ (Type(text, dri) :: suffix))
  def keyword(str: String): SignatureBuilder = copy(content = content :+ Keyword(str))
  def tpe(text: String, dri: DRI): SignatureBuilder = copy(content = content :+ Type(text, Some(dri)))
  def signature(s: Signature): SignatureBuilder = copy(content = content ++ s)

  def list[E](
    elements: Seq[E],
    prefix: Signature = List(Plain("")),
    suffix: Signature = List(Plain("")),
    separator: Signature = List(Plain(", ")),
    forcePrefixAndSuffix: Boolean = false
  )(
    elemOp: (SignatureBuilder, E) => SignatureBuilder
  ): SignatureBuilder = elements match {
    case Nil => if forcePrefixAndSuffix then signature(prefix).signature(suffix) else this
    case head :: tail =>
      tail.foldLeft(elemOp(signature(prefix), head))((b, e) => elemOp(b.signature(separator), e)).signature(suffix)
  }

  def annotationsBlock(d: Member): SignatureBuilder =
    d.annotations.foldLeft(this){ (bdr, annotation) => bdr.buildAnnotation(annotation)}

  def annotationsInline(d: TermParameter): SignatureBuilder =
    d.annotations.foldLeft(this){ (bdr, annotation) => bdr.buildAnnotation(annotation) }

  def annotationsInline(t: TypeParameter): SignatureBuilder =
    t.annotations.foldLeft(this){ (bdr, annotation) => bdr.buildAnnotation(annotation) }

  private def buildAnnotation(a: Annotation): SignatureBuilder =
    tpe(s"@${a.dri.location.split('.').last}", Some(a.dri)).buildAnnotationParams(a).plain(" ")

  private def buildAnnotationParams(a: Annotation): SignatureBuilder =
    if !a.params.isEmpty then
      val params = a.params.filterNot {
        case Annotation.LinkParameter(_, _, text) => text == "_"
        case _ => false
      }
      list(params, List(Plain("(")), List(Plain(")")), List(Plain(", "))){ (bdr, param) => bdr.buildAnnotationParameter(param)}
    else this

  private def addParameterName(txt: Option[String]): SignatureBuilder = txt match {
    case Some(name) => this.plain(s"$name = ")
    case _ => this
  }

  private def buildAnnotationParameter(a: Annotation.AnnotationParameter): SignatureBuilder = a match {
    case Annotation.PrimitiveParameter(name, value) =>
      addParameterName(name).plain(value)
    case Annotation.LinkParameter(name, dri, text) =>
      addParameterName(name).tpe(text, Some(dri))
    case Annotation.UnresolvedParameter(name, value) =>
      addParameterName(name).plain(value)
  }

  def parentsSignature(member: Member): SignatureBuilder =
    member.directParents match
      case Nil => this
      case extendType :: withTypes =>
        val extendPart = keyword(" extends ").signature(extendType.signature)
        withTypes.foldLeft(extendPart)((bdr2, tpe) => bdr2.keyword(", ").signature(tpe.signature))

  def modifiersAndVisibility(t: Member) =
    val (prefixMods, suffixMods) = t.modifiers.partition(_.prefix)
    val all = prefixMods.map(_.name) ++ Seq(t.visibility.asSignature) ++ suffixMods.map(_.name)
    val filtered = all.filter(_.trim.nonEmpty)
    if filtered.nonEmpty then keyword(filtered.toSignatureString()) else this

  def kind(k: Kind) =
    keyword(k.name + " ")


  def functionParameters(paramss: Seq[ Either[TermParameterList,TypeParameterList] ]) =
    this.list(paramss, separator = List(Plain(""))) {
      case (bld, Left(params: TermParameterList))  => bld.termParamList(params)
      case (bld, Right(params: TypeParameterList)) => bld.typeParamList(params)
    }

  def termParamList(params: TermParameterList) =
    this.list(params.parameters, prefix = List(Plain("("), Keyword(params.modifiers)), suffix = List(Plain(")")), forcePrefixAndSuffix = true) { (bld, p) =>
      val annotationsAndModifiers = bld.annotationsInline(p)
        .keyword(p.modifiers)
      val name = p.name.fold(annotationsAndModifiers)(annotationsAndModifiers.name(_, p.dri).plain(": "))
      name.signature(p.signature)
    }

  def typeParamList(on: TypeParameterList) = list(on.toList, List(Plain("[")), List(Plain("]"))){ (bdr, e) =>
    bdr.annotationsInline(e).keyword(e.variance).tpe(e.name, Some(e.dri), e.isCaptureVar).signature(e.signature)
  }

  def functionTermParameters(paramss: Seq[TermParameterList]) =
    this.list(paramss, separator = List(Plain(""))) { (bld, pList) => bld.termParamList(pList) }

trait ScalaSignatureUtils:
  extension (tokens: Seq[String]) def toSignatureString(): String =
    tokens.filter(_.trim.nonEmpty).mkString(""," "," ")
