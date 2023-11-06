package dotty.tools.dotc.core.tasty

import dotty.tools.tasty.TastyFormat.*

import scala.collection.immutable.BitSet

class Attributes private[tasty](
  private[tasty] val booleanTags: BitSet,
) {
  def scala2StandardLibrary: Boolean = booleanTags(SCALA2STANDARDLIBRARYattr)
  def explicitNulls: Boolean = booleanTags(EXPLICITNULLSattr)
  def captureChecked: Boolean = booleanTags(CAPTURECHECKEDattr)
  def withPureFuns: Boolean = booleanTags(WITHPUREFUNSattr)
  def isJava: Boolean = booleanTags(JAVAattr)
  def isOutline: Boolean = booleanTags(OUTLINEattr)
}

object Attributes:
  def apply(
    scala2StandardLibrary: Boolean,
    explicitNulls: Boolean,
    captureChecked: Boolean,
    withPureFuns: Boolean,
    isJava: Boolean,
    isOutline: Boolean,
  ): Attributes =
    val booleanTags = BitSet.newBuilder
    if scala2StandardLibrary then booleanTags += SCALA2STANDARDLIBRARYattr
    if explicitNulls then booleanTags += EXPLICITNULLSattr
    if captureChecked then booleanTags += CAPTURECHECKEDattr
    if withPureFuns then booleanTags += WITHPUREFUNSattr
    if isJava then booleanTags += JAVAattr
    if isOutline then booleanTags += OUTLINEattr
    new Attributes(booleanTags.result())
  end apply

  val empty: Attributes =
    new Attributes(BitSet.empty)
