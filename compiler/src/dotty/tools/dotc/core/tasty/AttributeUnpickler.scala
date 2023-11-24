package dotty.tools.dotc
package core.tasty

import scala.language.unsafeNulls

import dotty.tools.tasty.{TastyFormat, TastyReader, TastyBuffer}

class AttributeUnpickler(reader: TastyReader):
  import reader._

  lazy val attributes: Attributes = {
    val booleanTags = List.newBuilder[Int]

    while !isAtEnd do
      booleanTags += readByte()

    new Attributes(booleanTags.result())
  }

end AttributeUnpickler
