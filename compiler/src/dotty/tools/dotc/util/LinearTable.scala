package dotty.tools.dotc.util

/** A table with an immutable API that must be used linearly since it uses
 *  mutation internally. A packed array representation is used for sizes
 *  up to 8, and a hash table is used for larger sizes.
 */
abstract class LinearTable[Key >: Null <: AnyRef, Value >: Null <: AnyRef]:
  def lookup(key: Key): Value
  def enter(key: Key, value: Value): LinearTable[Key, Value]
  def invalidate(key: Key): Unit
  def size: Int

object LinearTable:
  def empty[Key >: Null <: AnyRef, Value >: Null <: AnyRef]: LinearTable[Key, Value] =
    ArrayTable[Key, Value](8)

class ArrayTable[Key >: Null <: AnyRef, Value >: Null <: AnyRef](capacity: Int) extends LinearTable[Key, Value]:

  val elems = new Array[AnyRef](capacity * 2)
  var size = 0

  def lookup(key: Key): Value =
    var i = 0
    while i < elems.length do
      if elems(i) eq key then
        return elems(i + 1).asInstanceOf[Value]
      if elems(i) == null then
        return null
      i += 2
    null

  def enter(key: Key, value: Value): LinearTable[Key, Value] =
    var i = 0
    while i < elems.length do
      if elems(i) eq key then
        elems(i + 1) = value
        return this
      if elems(i) == null then
        elems(i) = key
        elems(i + 1) = value
        size += 1
        return this
      i += 2
    val ht = HashTable[Key, Value](initialCapacity = 16)
    i = 0
    while i < elems.length do
      ht.enter(elems(i).asInstanceOf[Key], elems(i + 1).asInstanceOf[Value])
      i += 2
    ht.enter(key, value)
    ht

  def invalidate(key: Key): Unit =
    var i = 0
    while i < elems.length do
      if elems(i) eq key then
        size -= 1
        elems(i) = null
        return
      i += 2

  override def toString: String =
    val buf = new StringBuilder
    var i = 0
    while i < elems.length do
      buf.append(if i == 0 then "ArrayTable(" else ", ")
      if elems(i) != null then
        buf.append(elems(i))
        buf.append(" -> ")
        buf.append(elems(i + 1))
      i += 2
    buf.append(")")
    buf.toString
end ArrayTable

class HashTable[Key >: Null <: AnyRef, Value >: Null <: AnyRef](initialCapacity: Int) extends LinearTable[Key, Value]:
  private val table = java.util.HashMap[Key, Value](initialCapacity)

  def lookup(key: Key): Value =
    table.get(key)
  def enter(key: Key, value: Value): LinearTable[Key, Value] =
    table.put(key, value)
    this
  def invalidate(key: Key): Unit =
    table.remove(key)
  def size: Int =
    table.size

  override def toString: String = table.toString
end HashTable