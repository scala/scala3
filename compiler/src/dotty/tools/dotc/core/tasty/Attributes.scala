package dotty.tools.dotc.core.tasty

import dotty.tools.tasty.TastyFormat.*

import scala.collection.immutable.BitSet

class Attributes private[tasty](
  private[tasty] val booleanTags: BitSet,
) {
  def scala2StandardLibrary: Boolean = booleanTags(SCALA2STANDARDLIBRARYattr)
  def explicitNulls: Boolean = booleanTags(EXPLICITNULLSattr)
}

object Attributes:
  def apply(
    scala2StandardLibrary: Boolean,
    explicitNulls: Boolean,
  ): Attributes =
    val booleanTags = BitSet.newBuilder
    if scala2StandardLibrary then booleanTags += SCALA2STANDARDLIBRARYattr
    if explicitNulls then booleanTags += EXPLICITNULLSattr
    new Attributes(booleanTags.result())
  end apply

  val empty: Attributes =
    new Attributes(BitSet.empty)
