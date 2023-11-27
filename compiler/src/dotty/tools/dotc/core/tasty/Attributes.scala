package dotty.tools.dotc.core.tasty

import dotty.tools.tasty.TastyFormat

class Attributes(
  val booleanTags: List[Int],
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
    val booleanTags = List.newBuilder[Int]
    if scala2StandardLibrary then booleanTags += TastyFormat.SCALA2STANDARDLIBRARYattr
    if explicitNulls then booleanTags += TastyFormat.EXPLICITNULLSattr
    new Attributes(booleanTags.result())
  end apply
