package scala.collection.mutable

import org.junit.Assert.*
import org.junit.Test
import scala.collection.mutable

@annotation.nowarn("cat=deprecation&origin=scala.collection.mutable.AnyRefMap")
class SerializationTest {

  @Test
  def arrayBuffer(): Unit = {
    assertEqualsAfterDeserialization(mutable.ArrayBuffer.empty[Int], classOf[mutable.ArrayBuffer[?]])
    assertEqualsAfterDeserialization(mutable.ArrayBuffer(1, 2, 3), classOf[mutable.ArrayBuffer[?]])
  }

  @Test
  def listBuffer(): Unit = {
    assertEqualsAfterDeserialization(mutable.ListBuffer.empty[Int], classOf[mutable.ListBuffer[?]])
    assertEqualsAfterDeserialization(mutable.ListBuffer(1, 2, 3), classOf[mutable.ListBuffer[?]])
  }

  @Test
  def hashMap(): Unit = {
    assertEqualsAfterDeserialization(mutable.HashMap.empty[Int, String], classOf[mutable.HashMap[?, ?]])
    assertEqualsAfterDeserialization(mutable.HashMap(1 -> "one", 2 -> "two", 3 -> "three"), classOf[mutable.HashMap[?, ?]])
  }

  @Test
  def linkedHashMap(): Unit = {
    assertEqualsAfterDeserialization(mutable.LinkedHashMap.empty[Int, String], classOf[mutable.LinkedHashMap[?, ?]])
    assertEqualsAfterDeserialization(mutable.LinkedHashMap(1 -> "one", 2 -> "two", 3 -> "three"), classOf[mutable.LinkedHashMap[?, ?]])
  }

  @Test
  def longMap(): Unit = {
    assertEqualsAfterDeserialization(mutable.LongMap.empty[String], classOf[mutable.LongMap[?]])
    assertEqualsAfterDeserialization(mutable.LongMap(1L -> "one", 2L -> "two", 3L -> "three"), classOf[mutable.LongMap[?]])
  }

  @Test
  def treeMap(): Unit = {
    assertEqualsAfterDeserialization(mutable.TreeMap.empty[Int, String], classOf[mutable.TreeMap[?, ?]])
    assertEqualsAfterDeserialization(mutable.TreeMap(1 -> "one", 2 -> "two", 3 -> "three"), classOf[mutable.TreeMap[?, ?]])
  }

  @Test
  def anyRefMap(): Unit = {
    assertEqualsAfterDeserialization(mutable.AnyRefMap.empty[String, String], classOf[mutable.AnyRefMap[?, ?]])
    assertEqualsAfterDeserialization(mutable.AnyRefMap("1" -> "one", "2" -> "two", "3" -> "three"), classOf[mutable.AnyRefMap[?, ?]])
  }

  @Test
  def mapWithDefault(): Unit = {
    assertEqualsAfterDeserialization(mutable.Map.empty[Int, String].withDefaultValue("none"), classOf[mutable.Map[?, ?]])
    assertEqualsAfterDeserialization(mutable.Map(1 -> "one").withDefaultValue("none"), classOf[mutable.Map[?, ?]])
  }

  @Test
  def sortedMapWithDefault(): Unit = {
    assertEqualsAfterDeserialization(mutable.SortedMap.empty[Int, String].withDefaultValue("none"), classOf[mutable.SortedMap[?, ?]])
    assertEqualsAfterDeserialization(mutable.SortedMap(1 -> "one").withDefaultValue("none"), classOf[mutable.SortedMap[?, ?]])
  }

  @Test
  def hashSet(): Unit = {
    assertEqualsAfterDeserialization(mutable.HashSet.empty[Int], classOf[mutable.HashSet[?]])
    assertEqualsAfterDeserialization(mutable.HashSet(1, 2, 3), classOf[mutable.HashSet[?]])
  }

  @Test
  def linkedHashSet(): Unit = {
    assertEqualsAfterDeserialization(mutable.LinkedHashSet.empty[Int], classOf[mutable.LinkedHashSet[?]])
    assertEqualsAfterDeserialization(mutable.LinkedHashSet(1, 2, 3), classOf[mutable.LinkedHashSet[?]])
  }

  @Test
  def bitSet(): Unit = {
    assertEqualsAfterDeserialization(mutable.BitSet.empty, classOf[mutable.BitSet])
    assertEqualsAfterDeserialization(mutable.BitSet(1, 2, 3), classOf[mutable.BitSet])
  }

  @Test
  def treeSet(): Unit = {
    assertEqualsAfterDeserialization(mutable.TreeSet.empty[Int], classOf[mutable.TreeSet[?]])
    assertEqualsAfterDeserialization(mutable.TreeSet(1, 2, 3), classOf[mutable.TreeSet[?]])
  }

  @Test
  def priorityQueue(): Unit = {
    assertEquals(Seq(), serializeDeserialize(mutable.PriorityQueue.empty[Int]).toSeq)
    assertEquals(Seq(3, 2, 1), serializeDeserialize(mutable.PriorityQueue(1, 2, 3)).toSeq)
  }

  @Test
  def queue(): Unit = {
    assertEquals(Seq(), serializeDeserialize(mutable.Queue.empty[Int]).toSeq)
    assertEquals(Seq(1, 2, 3), serializeDeserialize(mutable.Queue(1, 2, 3)).toSeq)
  }

  private def assertEqualsAfterDeserialization[A](original: mutable.Iterable[A], expectedClass: Class[?]): Unit = {
    val after = serializeDeserialize(original)
    assertEquals(original, after)
    assertTrue("Deserialized class "+after.getClass.getName+" is not assignable to "+expectedClass.getName, expectedClass.isInstance(after))
  }

  private def serializeDeserialize[T <: AnyRef](obj: T): T = {
    import java.io.*
    val buffer = new ByteArrayOutputStream
    val out = new ObjectOutputStream(buffer)
    out.writeObject(obj)
    val in = new ObjectInputStream(new ByteArrayInputStream(buffer.toByteArray))
    in.readObject.asInstanceOf[T]
  }
}
