package dotty.tools.dotc.staging

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.StdNames.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.util.Property

import scala.collection.mutable.LinkedHashSet

object QuoteTypeTags:

  private val TaggedTypes = new Property.Key[LinkedHashSet[TermRef]]

  def inContextWithQuoteTypeTags(body: Context ?=> tpd.Tree)(using Context): (List[tpd.Tree], tpd.Tree) =
    val tags = LinkedHashSet.empty[TermRef]
    val transformed = body(using ctx.fresh.setProperty(TaggedTypes, tags))
    (tags.toList.map(tpd.ref(_)), transformed)

  def getTagRef(spliced: TermRef)(using Context): Type =
    ctx.property(TaggedTypes).get += spliced
    spliced.select(tpnme.Underlying)
