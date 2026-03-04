package dotty.tools.dotc.core.tasty

import dotty.tools.tasty.TastyFormat.*

import scala.collection.immutable.BitSet
import scala.collection.immutable.TreeMap

class Attributes private[tasty](
  private[tasty] val booleanTags: BitSet,
  private[tasty] val stringTagValues: List[(Int, String)],
) {
  def scala2StandardLibrary: Boolean = booleanTags(SCALA2STANDARDLIBRARYattr)
  def explicitNulls: Boolean = booleanTags(EXPLICITNULLSattr)
  def captureChecked: Boolean = booleanTags(CAPTURECHECKEDattr)
  def withPureFuns: Boolean = booleanTags(WITHPUREFUNSattr)
  def isJava: Boolean = booleanTags(JAVAattr)
  def isOutline: Boolean = booleanTags(OUTLINEattr)
  def sourceFile: Option[String] = stringTagValues.find(_._1 == SOURCEFILEattr).map(_._2)
}

object Attributes:
  def apply(
    sourceFile: String,
    scala2StandardLibrary: Boolean,
    explicitNulls: Boolean,
    captureChecked: Boolean,
    withPureFuns: Boolean,
    isJava: Boolean,
    isOutline: Boolean
  ): Attributes =
    val booleanTags = BitSet.newBuilder
    if scala2StandardLibrary then booleanTags += SCALA2STANDARDLIBRARYattr
    if explicitNulls then booleanTags += EXPLICITNULLSattr
    if captureChecked then booleanTags += CAPTURECHECKEDattr
    if withPureFuns then booleanTags += WITHPUREFUNSattr
    if isJava then booleanTags += JAVAattr
    if isOutline then booleanTags += OUTLINEattr

    val stringTagValues = List.newBuilder[(Int, String)]
    stringTagValues += SOURCEFILEattr -> sourceFile

    new Attributes(booleanTags.result(), stringTagValues.result())
  end apply

  val empty: Attributes =
    new Attributes(BitSet.empty, Nil)
