package scala.collection

import org.junit.Test

class ReusableBuildersTest {
  // GrowingBuilders are NOT reusable but can clear themselves
  @Test
  def test_SI8648(): Unit = {
    val b = collection.mutable.HashSet.newBuilder[Int]
    b += 3
    b.clear()
    assert(!b.isInstanceOf[collection.mutable.ReusableBuilder[?, ?]])
    assert(b.isInstanceOf[collection.mutable.GrowableBuilder[?, ?]])
    assert(b.result() == Set[Int]())
  }

  // ArrayBuilders ARE reusable, regardless of whether they returned their internal array or not
  @Test
  def test_SI9564(): Unit = {
    val b = Array.newBuilder[Float]
    b += 3f
    val three = b.result()
    b.clear()
    b ++= (1 to 16).map(_.toFloat)
    val sixteen = b.result()
    b.clear()
    b += 0f
    val zero = b.result()
    assert(b.isInstanceOf[collection.mutable.ReusableBuilder[?, ?]])
    assert(three.toList == 3 :: Nil)
    assert(sixteen.toList == (1 to 16))
    assert(zero.toList == 0 :: Nil)
  }

  @Test
  def test_reusability(): Unit = {
    val bl = List.newBuilder[String]
    val bv = Vector.newBuilder[String]
    val ba = collection.mutable.ArrayBuffer.newBuilder[String]
    assert(bl.isInstanceOf[collection.mutable.ReusableBuilder[?, ?]])
    assert(bv.isInstanceOf[collection.mutable.ReusableBuilder[?, ?]])
    assert(!ba.isInstanceOf[collection.mutable.ReusableBuilder[?, ?]])
  }
}
