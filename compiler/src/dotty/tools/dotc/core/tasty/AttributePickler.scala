package dotty.tools.dotc.core.tasty

import dotty.tools.dotc.ast.{tpd, untpd}

import dotty.tools.tasty.TastyBuffer
import dotty.tools.tasty.TastyFormat, TastyFormat.AttributesSection

object AttributePickler:

  def pickleAttributes(
    attributes: Attributes,
    pickler: TastyPickler,
    buf: TastyBuffer
  ): Unit =
    if attributes.booleanTags.nonEmpty then
      pickler.newSection(AttributesSection, buf)

    for tag <- attributes.booleanTags do
      buf.writeByte(tag)

  end pickleAttributes

end AttributePickler
