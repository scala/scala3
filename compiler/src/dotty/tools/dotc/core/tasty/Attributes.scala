package dotty.tools.dotc.core.tasty

import dotty.tools.tasty.TastyFormat

import scala.collection.immutable.BitSet

class Attributes(
  val booleanTags: BitSet,
) {
  def scala2StandardLibrary: Boolean =
    booleanTags.contains(TastyFormat.SCALA2STANDARDLIBRARYattr)
  def explicitNulls: Boolean =
    booleanTags.contains(TastyFormat.EXPLICITNULLSattr)
}

object Attributes:
  def apply(
    scala2StandardLibrary: Boolean,
    explicitNulls: Boolean,
  ): Attributes =
    val booleanTags = BitSet.newBuilder
    if scala2StandardLibrary then booleanTags += TastyFormat.SCALA2STANDARDLIBRARYattr
    if explicitNulls then booleanTags += TastyFormat.EXPLICITNULLSattr
    new Attributes(booleanTags.result())
  end apply

  def empty: Attributes =
    new Attributes(BitSet.empty)
