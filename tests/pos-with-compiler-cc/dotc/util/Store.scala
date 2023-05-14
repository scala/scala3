package dotty.tools.dotc.util

object Store {

  class Location[T](private[Store] val idx: Int) extends AnyVal

  val empty: Store = new Store(Array())
}

class Store(private val elems: Array[AnyRef | Null]) extends AnyVal {
  import Store._

  def newLocation[T](): (Location[T], Store) = {
    val elems1 = new Array[AnyRef | Null](elems.length + 1)
    System.arraycopy(elems, 0, elems1, 0, elems.length)
    (new Location(elems.length), new Store(elems1))
  }

  def newLocation[T](initial: T): (Location[T], Store) = {
    val (loc, store) = newLocation[T]()
    store.elems(loc.idx) = initial.asInstanceOf[AnyRef | Null]
    (loc, store)
  }

  def updated[T](loc: Location[T], value: T): Store = {
    val elems1 = elems.clone
    elems1(loc.idx) = value.asInstanceOf[AnyRef | Null]
    new Store(elems1)
  }

  def apply[T](loc: Location[T]): T =
    elems(loc.idx).asInstanceOf[T]
}
