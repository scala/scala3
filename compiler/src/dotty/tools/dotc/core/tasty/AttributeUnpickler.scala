package dotty.tools.dotc
package core.tasty

import scala.language.unsafeNulls
import scala.collection.immutable.BitSet

import dotty.tools.tasty.{TastyFormat, TastyReader, TastyBuffer}

class AttributeUnpickler(reader: TastyReader):
  import reader._

  lazy val attributes: Attributes = {
    val booleanTags = BitSet.newBuilder

    while !isAtEnd do
      booleanTags += readByte()

    new Attributes(booleanTags.result())
  }

end AttributeUnpickler
