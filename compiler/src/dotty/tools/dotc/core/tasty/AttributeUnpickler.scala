package dotty.tools.dotc
package core.tasty

import scala.language.unsafeNulls
import scala.collection.immutable.BitSet
import scala.collection.immutable.TreeMap

import dotty.tools.tasty.{TastyFormat, TastyReader, TastyBuffer}, TastyFormat.{isBooleanAttrTag, isStringAttrTag}
import dotty.tools.dotc.core.tasty.TastyUnpickler.NameTable

class AttributeUnpickler(reader: TastyReader, nameAtRef: NameTable):
  import reader._

  lazy val attributes: Attributes = {
    val booleanTags = BitSet.newBuilder
    val stringTagValue = List.newBuilder[(Int, String)]

    var lastTag = -1
    while !isAtEnd do
      val tag = readByte()
      if isBooleanAttrTag(tag) then
        booleanTags += tag
      else if isStringAttrTag(tag) then
        val utf8Ref = readNameRef()
        val value = nameAtRef(utf8Ref).toString
        stringTagValue += tag -> value
      else
        assert(false, "unknown attribute tag: " + tag)

      assert(tag != lastTag, s"duplicate attribute tag: $tag")
      assert(tag > lastTag, s"attribute tags are not ordered: $tag after $lastTag")
      lastTag = tag
    end while

    new Attributes(booleanTags.result(), stringTagValue.result())
  }

end AttributeUnpickler
