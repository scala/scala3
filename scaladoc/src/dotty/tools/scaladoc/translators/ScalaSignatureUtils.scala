package dotty.tools.scaladoc
package translators

case class InlineSignatureBuilder(names: Signature = Nil, preName: Signature = Nil) extends SignatureBuilder:
  override def plain(str: String): SignatureBuilder = copy(names = Plain(str) +: names)
  override def name(str: String, dri: DRI): SignatureBuilder = copy(names = Nil, preName = names)
  override def tpe(text: String, dri: Option[DRI]): SignatureBuilder = copy(names = Type(text, dri) +: names)
  override def keyword(str: String): SignatureBuilder = copy(names = Keyword(str) +: names)
  def tpe(text: String, dri: DRI): SignatureBuilder = copy(names = Type(text, Some(dri)) +: names)
  override def signature(s: Signature): SignatureBuilder = copy(names = s.reverse ++ names)

object InlineSignatureBuilder:
  def typeSignatureFor(d: Member): Signature =
      ScalaSignatureProvider.rawSignature(d, InlineSignatureBuilder()).asInstanceOf[InlineSignatureBuilder].names.reverse

trait SignatureBuilder extends ScalaSignatureUtils {
  def keyword(str: String): SignatureBuilder
  def plain(str: String): SignatureBuilder
  def name (str: String, dri: DRI): SignatureBuilder
  def tpe(str: String, dri: Option[DRI]): SignatureBuilder
  def signature(s: Signature): SignatureBuilder = s.foldLeft(this){
    case (bld, Type(a, b)) => bld.tpe(a, b)
    case (bld, Keyword(a)) => bld.keyword(a)
    case (bld, Plain(a)) => bld.plain(a)
  }

  // Support properly once we rewrite signature builder
  def memberName(name: String, dri: DRI) = plain(name)

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

  def modifiersAndVisibility(t: Member, kind: String) =
    val (prefixMods, suffixMods) = t.modifiers.partition(_.prefix)
    val all = prefixMods.map(_.name) ++ Seq(t.visibility.asSignature) ++ suffixMods.map(_.name)
    val filtered = all.filter(_.trim.nonEmpty)
    val intermediate = if filtered.nonEmpty then keyword(filtered.toSignatureString()) else this
    intermediate.keyword(kind + " ")

  def generics(on: Seq[TypeParameter]) = list(on.toList, List(Plain("[")), List(Plain("]"))){ (bdr, e) =>
    bdr.annotationsInline(e).keyword(e.variance).tpe(e.name, Some(e.dri)).signature(e.signature)
  }

  def functionParameters(params: Seq[TermParametersList]) =
    if params.isEmpty then this.plain("")
    else if params.size == 1 && params(0).parameters == Nil then this.plain("()")
    else this.list(params, separator = List(Plain(""))) { (bld, pList) =>
      bld.list(pList.parameters, prefix = List(Plain("("), Keyword(pList.modifiers)), suffix = List(Plain(")")), forcePrefixAndSuffix = true) { (bld, p) =>
        val annotationsAndModifiers = bld.annotationsInline(p)
          .keyword(p.modifiers)
        val name = p.name.fold(annotationsAndModifiers)(annotationsAndModifiers.memberName(_, p.dri).plain(": "))
        name.signature(p.signature)
      }
    }
  /* 
  def functionParameter = 
    (bdr: SignatureBuilder, param: TypeParametersList | TermParametersList) => param match {
      case types: TypeParameterList => generics(ts)
      case terms: TermParametersList => functionParameters(terms)
    }  */

  def functionParameters2(params: Seq[TypeParametersList | TermParametersList]) = params.foldLeft(this){ (bdr, params) => params match {
      case types: TypeParametersList => bdr.generics(types.parameters)
      case terms: TermParametersList => bdr.functionParameters(Seq(terms))
    }} // TODO: sc
    
  
}



trait ScalaSignatureUtils:
  extension (tokens: Seq[String]) def toSignatureString(): String =
    tokens.filter(_.trim.nonEmpty).mkString(""," "," ")
