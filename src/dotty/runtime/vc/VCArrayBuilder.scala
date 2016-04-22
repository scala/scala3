package dotty.runtime.vc

import scala.collection.TraversableOnce
import scala.collection.mutable.ArrayBuilder
import scala.reflect.ClassTag

class VCArrayBuilder[T](implicit ct: ClassTag[T]) extends ArrayBuilder[T] {

  private var elems: Array[T] = _
  private var capacity: Int = 0
  private var size: Int = 0

  private def mkArray(size: Int): Array[T] = {
    val newelems = new Array[T](size)
    if (this.size > 0) Array.copy(elems, 0, newelems, 0, this.size)
    newelems
  }

  private def resize(size: Int) = {
    elems = mkArray(size)
    capacity = size
  }

  override def sizeHint(size: Int) = {
    if (capacity < size) resize(size)
  }

  private def ensureSize(size: Int) = {
    if (capacity < size || capacity == 0) {
      var newsize = if (capacity == 0) 16 else capacity * 2
      while (newsize < size) newsize *= 2
      resize(newsize)
    }
  }

  def +=(elem: T): this.type = {
    ensureSize(size + 1)
    elems(size) = elem
    size += 1
    this
  }

  override def ++=(xs: TraversableOnce[T]): this.type = (xs.asInstanceOf[AnyRef]) match {
    case xs: VCWrappedArray[_] =>
      ensureSize(this.size + xs.length)
      //TODO: check correctness
      Array.copy(xs.array, 0, elems, this.size, xs.length)
      size += xs.length
      this
    case _ =>
      super.++=(xs)
  }

  def clear() = {
    size = 0
  }

  def result() = {
    if (capacity != 0 && capacity == size) elems
    else mkArray(size)
  }

  override def equals(other: Any): Boolean = other match {
    case x: VCArrayBuilder[_] => (size == x.size) && (elems == x.elems)
    case _ => false
  }

  override def toString = "ArrayBuilder.ofVC"
}