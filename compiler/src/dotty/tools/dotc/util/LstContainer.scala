package dotty.tools.dotc.util

import reflect.ClassTag
import util.hashing.MurmurHash3

/** A super-trait for case classes containing `Lst` elements
 *  which defines the correct `equals`, `hashCode` and `toString` methods so
 *  that the functionality is the same as if the elements were regular Lists.
 *  @param T   the case class
 */
trait LstContainer[T](using ct: ClassTag[T]) { self: Product =>
  override def toString =
    val it = productIterator
    val sb = StringBuilder() ++= productPrefix += '('
    var first = true
    while it.hasNext do
      if !first then sb += ','
      first = false
      sb ++= it.next().match
        case elem: Array[Object] => elem.asInstanceOf[Lst[Object]]._toString
        case elem => elem.toString
    sb += ')'
    sb.toString

  override def equals(that: Any): Boolean =
    (this `eq` that.asInstanceOf[Object])
    || ct.runtimeClass.isInstance(that)
        && {
          val that1 = that.asInstanceOf[Product]
          var i = 0
          var len = productArity
          while i < len
            && productElement(i).match
              case elem: Array[Object] =>
                elem.asInstanceOf[Lst[Object]] === that1.productElement(i).asInstanceOf[Lst[Object]]
              case elem =>
                elem == that1.productElement(i)
          do i += 1
          i == len && that1.canEqual(this)
        }

  override def hashCode: Int =
    val len = productArity
    var h = productPrefix.hashCode
    var i = 0
    while i < len do
      val elemHash = productElement(i) match
        case elem: Array[Object] => java.util.Arrays.hashCode(elem)
        case elem => elem.##
      h = MurmurHash3.mix(h, elemHash)
      i += 1
    MurmurHash3.finalizeHash(h, len)
}


