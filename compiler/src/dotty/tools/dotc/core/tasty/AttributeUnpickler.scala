package dotty.tools.dotc
package core.tasty

import scala.language.unsafeNulls

import dotty.tools.tasty.{TastyReader, TastyBuffer}

import java.nio.charset.StandardCharsets

class AttributeUnpickler(reader: TastyReader):
  import reader._

  private[tasty] lazy val attributes: List[String] = {
    val attributesBuilder = List.newBuilder[String]
    while (!isAtEnd) {
      val length = readNat()
      if (length > 0) {
        val bytes = readBytes(length)
        val attribute = new String(bytes, StandardCharsets.UTF_8)
        attributesBuilder += attribute
      }
    }
    attributesBuilder.result()
  }

end AttributeUnpickler
