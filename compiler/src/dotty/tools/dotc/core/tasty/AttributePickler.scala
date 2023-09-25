package dotty.tools.dotc.core.tasty

import dotty.tools.dotc.ast.{tpd, untpd}

import dotty.tools.tasty.TastyBuffer
import dotty.tools.tasty.TastyFormat.AttributesSection

import java.nio.charset.StandardCharsets

object AttributePickler:

  def pickleAttributes(
    attributes: List[String],
    pickler: TastyPickler,
    buf: TastyBuffer): Unit =
      if attributes != Nil then
        pickler.newSection(AttributesSection, buf)
        for attribute <- attributes do
          val bytes = attribute.getBytes(StandardCharsets.UTF_8).nn
          val length = bytes.length
          buf.writeNat(length)
          buf.writeBytes(bytes, length)
  end pickleAttributes

end AttributePickler
