package dotty.tools.pc

import java.net.URI

import scala.collection.mutable

class CompileUnitsCache(keepLastCount: Short):
  private val lastCompiled = new LastNElementsSet[String](keepLastCount)
  private val lastModified = mutable.Set[String]()

  def didGetUnit(uri: URI): Option[String] =
    lastCompiled
      .add(uri.toString)
      .filterNot(lastModified(_))

  def didChange(uri: URI): Unit =
    lastModified.add(uri.toString())

end CompileUnitsCache

/** A set collection that keeps the last N elements.
 *  @param keepLastCount the number of elements to keep
 */
class LastNElementsSet[T](keepLastCount: Short):
  private val cache = new mutable.LinkedHashSet[T]()
  private var last: Option[T] = None

  /** Add an element to the set.
   *  @param element the element to add
   *  @return the optional oldest element, that was removed from the set
   */
  def add(element: T): Option[T] = this.synchronized:
    if last.contains(element) then None
    else
      last = Some(element)
      if cache.contains(element) then
        cache.remove(element)
        cache.add(element)
        None
      else if cache.size >= keepLastCount then
        val oldest = cache.head
        cache.remove(oldest)
        cache.add(element)
        Some(oldest)
      else
        cache.add(element)
        None

  def contains(element: T): Boolean = cache.contains(element)

end LastNElementsSet
