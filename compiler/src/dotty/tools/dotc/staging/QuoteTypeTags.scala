package dotty.tools.dotc.staging

import dotty.tools.dotc.ast.{tpd, untpd}
import dotty.tools.dotc.core.Annotations._
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.NameKinds._
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.staging.StagingLevel.*
import dotty.tools.dotc.util.Property
import dotty.tools.dotc.util.Spans._

object QuoteTypeTags {

  private val TaggedTypes = new Property.Key[QuoteTypeTags]

  def contextWithQuoteTypeTags(taggedTypes: QuoteTypeTags)(using Context) =
    ctx.fresh.setProperty(TaggedTypes, taggedTypes)

  def getQuoteTypeTags(using Context): QuoteTypeTags =
    ctx.property(TaggedTypes).get
}

class QuoteTypeTags(span: Span)(using Context) {
  import tpd.*

  private val tags = collection.mutable.LinkedHashMap.empty[TermRef, TypeDef]

  def getTagRef(spliced: TermRef): TypeRef = {
    val typeDef = tags.getOrElseUpdate(spliced, mkTagSymbolAndAssignType(spliced))
    typeDef.symbol.typeRef
  }

  def getTypeTags: List[TypeDef] = tags.valuesIterator.toList

  private def mkTagSymbolAndAssignType(spliced: TermRef): TypeDef = {
    val splicedTree = tpd.ref(spliced).withSpan(span)
    val rhs = splicedTree.select(tpnme.Underlying).withSpan(span)
    val alias = ctx.typeAssigner.assignType(untpd.TypeBoundsTree(rhs, rhs), rhs, rhs, EmptyTree)
    val local = newSymbol(
      owner = ctx.owner,
      name = UniqueName.fresh(rhs.tpe.dealias.typeSymbol.name.toTypeName),
      flags = Synthetic,
      info = TypeAlias(splicedTree.tpe.select(tpnme.Underlying)),
      coord = span).asType
    local.addAnnotation(Annotation(defn.QuotedRuntime_SplicedTypeAnnot, span))
    ctx.typeAssigner.assignType(untpd.TypeDef(local.name, alias), local)
  }
}
