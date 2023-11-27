package dotty.tools.dotc
package core.tasty

import scala.language.unsafeNulls
import scala.collection.immutable.BitSet
import scala.collection.immutable.TreeMap

import dotty.tools.tasty.{TastyFormat, TastyReader, TastyBuffer}

class AttributeUnpickler(reader: TastyReader):
  import reader._

  lazy val attributes: Attributes = {
    val booleanTags = BitSet.newBuilder
    val stringTagValue = List.newBuilder[(Int, String)]

    while !isAtEnd do
      val tag = readByte()
      if tag < TastyFormat.firstStringAttrTag then
        booleanTags += tag
      else if tag < TastyFormat.firstUnassignedAttrTag then
        stringTagValue += tag -> readUtf8()
      else
        assert(false, "unknown attribute tag: " + tag)

    new Attributes(booleanTags.result(), stringTagValue.result())
  }

end AttributeUnpickler
