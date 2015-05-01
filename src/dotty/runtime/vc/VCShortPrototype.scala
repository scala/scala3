package dotty.runtime.vc

import scala.reflect.ClassTag

import scala.runtime.Statics

abstract class VCShortPrototype(val underlying: Short) extends VCPrototype {}

abstract class VCShortCasePrototype(underlying: Short) extends VCShortPrototype(underlying) with Product1[Short] {

  final def _1: Short = underlying

  override final def hashCode(): Int = {
    underlying.hashCode()
  }

  override final def toString: String = {
    s"$productPrefix($underlying)"
  }

  // subclasses are expected to implement equals, productPrefix, and canEqual
}

abstract class VCShortCompanion[T <: VCShortPrototype] extends ClassTag[T] {
  def box(underlying: Short): T
  final def unbox(boxed: T) = boxed.underlying
}

final class VCArrayShort[T <: VCShortPrototype](val ct: VCShortCompanion[T], sz: Int) extends VCArrayPrototype[T] {
  val arr = new Array[Short](sz)
  def apply(idx: Int) =
    ct.box(arr(idx))
  def update(idx: Int, elem: T) =
    arr(idx) = ct.unbox(elem)
  def length: Int = arr.length

  override def toString: String = {
    "[" + ct.runtimeClass
  }

  // Todo: what was the reason for 255 classes in my original proposal? arr.toString!
  // todo: need to discuss do we want to be compatible with ugly format of jvm here?
}
