package dotty.tools.dotc.util

import org.junit.Test
import org.junit.Assert.*

class EqHashMapTest:

  var counter = 0

  // basic identity hash, and reference equality, but with a counter for ordering
  class Id:
    val count = { counter += 1; counter }

  val id1, id2, id3 = Id()

  given Ordering[Id] = Ordering.by(_.count)

  @Test
  def invariant: Unit =
    assert((id1 ne id2) && (id1 ne id3) && (id2 ne id3))

  @Test
  def newEmpty: Unit =
    val m = EqHashMap[Id, Int]()
    assert(m.size == 0)
    assert(m.iterator.toList == Nil)

  @Test
  def update: Unit =
    val m = EqHashMap[Id, Int]()
    assert(m.size == 0 && !m.contains(id1))
    m.update(id1, 1)
    assert(m.size == 1 && m(id1) == 1)
    m.update(id1, 2) // replace value
    assert(m.size == 1 && m(id1) == 2)
    m.update(id3, 3) // new key
    assert(m.size == 2 && m(id1) == 2 && m(id3) == 3)

  @Test
  def getOrElseUpdate: Unit =
    val m = EqHashMap[Id, Int]()
    // add id1
    assert(m.size == 0 && !m.contains(id1))
    val added = m.getOrElseUpdate(id1, 1)
    assert(added == 1 && m.size == 1 && m(id1) == 1)
    // try add id1 again
    val addedAgain = m.getOrElseUpdate(id1, 23)
    assert(addedAgain != 23 && m.size == 1 && m(id1) == 1) // no change

  private def fullMap() =
    val m = EqHashMap[Id, Int]()
    m.update(id1, 1)
    m.update(id2, 2)
    m

  @Test
  def remove: Unit =
    val m = fullMap()
    // remove id2
    m.remove(id2)
    assert(m.size == 1)
    assert(m.contains(id1) && !m.contains(id2))
    // remove id1
    m -= id1
    assert(m.size == 0)
    assert(!m.contains(id1) && !m.contains(id2))

  @Test
  def lookup: Unit =
    val m = fullMap()
    assert(m.lookup(id1) == 1)
    assert(m.lookup(id2) == 2)
    assert(m.lookup(id3) == null)

  @Test
  def iterator: Unit =
    val m = fullMap()
    assert(m.iterator.toList.sorted == List(id1 -> 1,id2 -> 2))

  @Test
  def clear: Unit =
    locally:
      val s1 = fullMap()
      s1.clear()
      assert(s1.size == 0)
    locally:
      val s2 = fullMap()
      s2.clear(resetToInitial = false)
      assert(s2.size == 0)

  // basic structural equality and hash code
  class I32(val x: Int):
    override def hashCode(): Int = x
    override def equals(that: Any): Boolean = that match
      case that: I32 => this.x == that.x
      case _ => false

  /** the hash set is based on reference equality, i.e. does not use universal equality */
  @Test
  def referenceEquality: Unit =
    val i1, i2 = I32(1) // different instances

    assert(i1.equals(i2)) // structural equality
    assert(i1 ne i2) // reference inequality

    val m = locally:
      val m = EqHashMap[I32, Int]()
      m(i1) = 23
      m(i2) = 29
      m

    assert(m.size == 2 && m(i1) == 23 && m(i2) == 29)
    assert(m.keysIterator.toSet == Set(i1)) // scala.Set delegates to universal equality
  end referenceEquality

