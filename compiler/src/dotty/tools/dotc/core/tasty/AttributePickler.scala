package dotty.tools.dotc.core.tasty

import dotty.tools.dotc.ast.{tpd, untpd}

import dotty.tools.tasty.TastyBuffer
import dotty.tools.tasty.TastyFormat.*

object AttributePickler:

  def pickleAttributes(
    attributes: Attributes,
    pickler: TastyPickler,
    buf: TastyBuffer
  ): Unit =
    pickler.newSection(AttributesSection, buf)

    for tag <- attributes.booleanTags do
      assert(isBooleanAttrTag(tag), "Not a boolean attribute tag: " + tag)
      buf.writeByte(tag)

    assert(attributes.stringTagValues.exists(_._1 == SOURCEFILEattr))
    for (tag, value) <- attributes.stringTagValues do
      assert(isStringAttrTag(tag), "Not a string attribute tag: " + tag)
      val utf8Ref = pickler.nameBuffer.utf8Index(value)
      buf.writeByte(tag)
      buf.writeNat(utf8Ref.index)

  end pickleAttributes

end AttributePickler
