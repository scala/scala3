package dotty.runtime.vc

import scala.reflect.ClassTag

import scala.runtime.Statics

abstract class VCBooleanPrototype(val underlying: Boolean) extends VCPrototype {}

abstract class VCBooleanCasePrototype(underlying: Boolean) extends VCBooleanPrototype(underlying) with Product1[Boolean] {

  final def _1: Boolean = underlying

  override final def hashCode(): Int = {
    underlying.hashCode()
  }

  override final def toString: String = {
    s"$productPrefix($underlying)"
  }

  // subclasses are expected to implement equals, productPrefix, and canEqual
}

abstract class VCBooleanCompanion[T <: VCBooleanPrototype] extends ClassTag[T] {
  def box(underlying: Boolean): T
  final def unbox(boxed: T) = boxed.underlying
}

final class VCArrayBoolean[T <: VCBooleanPrototype](val ct: VCBooleanCompanion[T], sz: Int) extends VCArrayPrototype[T] {
  val arr = new Array[Boolean](sz)
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
