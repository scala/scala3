package dotty.tools.dotc.core.tasty
import dotty.tools.tasty.TastyBuffer
import collection.mutable

class ScratchData:
  var delta, delta1 = new Array[Int](0)

  val positionBuffer = new TastyBuffer(5000)
  val pickledIndices = new mutable.BitSet

  val commentBuffer = new TastyBuffer(5000)
  val attributeBuffer = new TastyBuffer(32)

  def reset() =
    assert(delta ne delta1)
    assert(delta.length == delta1.length)
    positionBuffer.reset()
    pickledIndices.clear()
    commentBuffer.reset()
    attributeBuffer.reset()

