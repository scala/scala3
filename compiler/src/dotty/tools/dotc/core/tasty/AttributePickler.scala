package dotty.tools.dotc.core.tasty

import dotty.tools.dotc.ast.{tpd, untpd}

import dotty.tools.tasty.TastyBuffer
import dotty.tools.tasty.TastyFormat, TastyFormat.AttributesSection

import java.nio.charset.StandardCharsets

object AttributePickler:

  def pickleAttributes(
    attributes: Attributes,
    pickler: TastyPickler,
    buf: TastyBuffer
  ): Unit =
    if attributes.scala2StandardLibrary || attributes.explicitNulls then // or any other attribute is set
      pickler.newSection(AttributesSection, buf)
      // Pickle attributes
      if attributes.scala2StandardLibrary then buf.writeNat(TastyFormat.SCALA2STANDARDLIBRARYattr)
      if attributes.explicitNulls then buf.writeNat(TastyFormat.EXPLICITNULLSattr)
    end if

  end pickleAttributes

end AttributePickler
