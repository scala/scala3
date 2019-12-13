package dotty.tools.dotc.semanticdb.internal

object WireFormat {
  val WIRETYPE_VARINT = 0
  val WIRETYPE_FIXED64 = 1
  val WIRETYPE_LENGTH_DELIMITED = 2
  val WIRETYPE_START_GROUP = 3
  val WIRETYPE_END_GROUP = 4
  val WIRETYPE_FIXED32 = 5

  val TAG_TYPE_BITS = 3
  val TAG_TYPE_MASK = (1 << TAG_TYPE_BITS) - 1

  def getTagWireType(tag: Int) = tag & TAG_TYPE_MASK

  def makeTag(fieldNumber: Int, wireType: Int) =
    (fieldNumber << TAG_TYPE_BITS) | wireType

  def getTagFieldNumber(tag: Int): Int = tag >>> TAG_TYPE_BITS
}
