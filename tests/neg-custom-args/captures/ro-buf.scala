import caps.unsafe.*
import caps.Stateful

class Buf[A] extends Stateful:
  this: Buf[A]^ =>
  val elems: Array[A]^ = ???
  def +=(elem: A): this.type =
    elems(0) = elem // error
    this

  /** Concrete collection type: ArrayBuffer */
  class ArrayBuffer[A] private (@untrackedCaptures initElems: Array[AnyRef]^, initLength: Int) {
    this: ArrayBuffer[A] =>
    def this() = this(new Array[AnyRef](16), 0)
    @untrackedCaptures private var elems: Array[AnyRef]^ = initElems
    @untrackedCaptures private var start = 0
    @untrackedCaptures private var end = initLength
    def apply(n: Int) = elems(start + n).asInstanceOf[A]
    def iterator: Iterator[A] = ???
    def length = end - start
    def +=(elem: A): this.type = {
      if (end == elems.length) {
        if (start > 0) {
          Array.copy(elems, start, elems, 0, length)
          end -= start
          start = 0
        }
        else {
          val newelems = new Array[AnyRef](end * 2)
          Array.copy(elems, 0, newelems, 0, end)
          elems = newelems
        }
      }
      elems(end) = elem.asInstanceOf[AnyRef] // error
        // cannot call update method on Array[A] since it is read-only.
        // elems is of type Array[A] since the self type is pure, so this
        // has type ArrayBuffer[A], which means that this.elems is read-only.
      end += 1
      this
    }
    override def toString = s"ArrayBuffer(${elems.slice(start, end).mkString(", ")})"
  }

