package dotty.tools.dotc.util

import org.junit.Test
import org.junit.Assert.*

class MutableIdentitySetTest:

  class Id(val name: String):
    // structural equality on purpose: the set must use reference identity
    override def equals(other: Any): Boolean = other match
      case that: Id => this.name == that.name
      case _ => false
    override def hashCode: Int = name.hashCode

  val id1 = Id("a")
  val id1b = Id("a") // structurally equal to id1, but a different reference
  val id2 = Id("b")
  val id3 = Id("c")

  @Test
  def newEmpty: Unit =
    val s = MutableIdentitySet[Id]()
    assert(s.isEmpty)
    assert(s.size == 0)
    assert(!s.contains(id1))
    assert(s.toList == Nil)

  @Test
  def addUsesIdentity: Unit =
    val s = MutableIdentitySet[Id]()
    s += id1
    s += id1b // structurally equal, different reference: must be a distinct element
    s += id2
    assert(s.size == 3)
    assert(s.contains(id1))
    assert(s.contains(id1b))
    assert(s.contains(id2))
    assert(!s.contains(id3))

  @Test
  def addIsIdempotentForSameReference: Unit =
    val s = MutableIdentitySet[Id]()
    s += id1
    s += id1
    assert(s.size == 1)
    s += id2
    s += id2
    assert(s.size == 2)

  @Test
  def preservesInsertionOrder: Unit =
    val s = MutableIdentitySet[Id]()
    s += id2
    s += id1
    s += id3
    assert(s.toList.map(_.name) == List("b", "a", "c"))
    // re-adding an existing element does not change the order
    s += id1
    assert(s.toList.map(_.name) == List("b", "a", "c"))

  @Test
  def remove: Unit =
    val s = MutableIdentitySet[Id]()
    s += id1
    s += id2
    s += id3
    s -= id2
    assert(s.size == 2)
    assert(!s.contains(id2))
    assert(s.contains(id1) && s.contains(id3))
    // removing a non-element is a no-op
    s -= Id("x")
    assert(s.size == 2)
    // removing everything
    s -= id1
    s -= id3
    assert(s.isEmpty)

  @Test
  def removeAndReaddKeepsSemantics: Unit =
    val s = MutableIdentitySet[Id]()
    s += id1
    s += id2
    s -= id1
    s += id1
    assert(s.size == 2)
    assert(s.contains(id1) && s.contains(id2))

  @Test
  def growsBeyondInitialCapacity: Unit =
    val s = MutableIdentitySet[Id]()
    val ids = (1 to 1000).map(i => Id(i.toString)).toList
    for id <- ids do s += id
    assert(s.size == 1000)
    for id <- ids do assert(s.contains(id))
    assert(s.toList.size == 1000)
    // order preserved
    assert(s.toList.map(_.name) == ids.map(_.name))
    // remove a spread of elements, then verify the survivors
    for i <- 0 until 1000 by 3 do s -= ids(i)
    assert(s.size == 1000 - 334)
    for i <- 0 until 1000 do
      assert(s.contains(ids(i)) == (i % 3 != 0))
    // re-add removed elements
    for i <- 0 until 1000 by 3 do s += ids(i)
    assert(s.size == 1000)

  @Test
  def forallAndFoldLeft: Unit =
    val s = MutableIdentitySet[Id]()
    s += id1
    s += id2
    s += id3
    assert(s.forall(_.name.length == 1))
    assert(!s.forall(_.name == "a"))
    assert(s.foldLeft(0)((acc, id) => acc + id.name.length) == 3)
    var seen = 0
    s.foreach(_ => seen += 1)
    assert(seen == 3)

  @Test
  def compactionKeepsProbingWorking: Unit =
    // Force several compaction rounds (removal-heavy) so the table is rebuilt at
    // sizes that must remain powers of two for the open-addressing probe to work.
    val s = MutableIdentitySet[Id]()
    val ids = (1 to 40).map(i => Id(i.toString)).toList
    for id <- ids do s += id
    // Remove most elements to trigger compaction with a small live set.
    for i <- 1 to 37 do s -= ids(i)
    assert(s.size == 3)
    assert(s.contains(ids(0)) && s.contains(ids(38)) && s.contains(ids(39)))
    assert(!s.contains(ids(1)))
    // Re-add and verify membership and order are still correct.
    for i <- 1 to 37 do s += ids(i)
    assert(s.size == 40)
    for id <- ids do assert(s.contains(id))

  @Test
  def clear: Unit =
    val s = MutableIdentitySet[Id]()
    s += id1
    s += id2
    s.clear()
    assert(s.isEmpty)
    s += id3
    assert(s.size == 1 && s.contains(id3))

  @Test
  def mutableSetInterface: Unit =
    val s = MutableIdentitySet[Id]()
    // add reports whether the element was new
    assert(s.add(id1))
    assert(!s.add(id1))
    // put returns the argument and inserts if absent
    assert(s.put(id2) eq id2)
    assert(s.put(id1) eq id1)
    assert(s.size == 2)
    // lookup returns the stored element or null
    assert(s.lookup(id2) eq id2)
    assert(s.lookup(id3) == null)
    assert(s.lookup(Id("b")) == null) // structural equality is ignored
    // iterator follows insertion order
    s ++= List(id3, id1)
    assert(s.iterator.map(_.name).toList == List("a", "b", "c"))
    s --= List(id2, Id("x"))
    assert(s.toList.map(_.name) == List("a", "c"))
    // clear(false) keeps the arrays but resets the contents
    s.clear(resetToInitial = false)
    assert(s.isEmpty)
    s += id1
    assert(s.toList.map(_.name) == List("a"))

end MutableIdentitySetTest
