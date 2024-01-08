package scala.collection
package mutable

object UnrolledBufferTest:
  def test(): Unit =
    new UnrolledBuffer.Unrolled[Int](0, Array.empty[Int], null, null)
    new UnrolledBuffer.Unrolled[Int](0, Array.empty[Int], null) // using default null argument
