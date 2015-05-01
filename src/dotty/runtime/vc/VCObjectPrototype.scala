package dotty.runtime.vc

import scala.reflect.ClassTag

import scala.runtime.Statics

abstract class VCObjectPrototype(val underlying: Object) extends VCPrototype {}

abstract class VCObjectCasePrototype(underlying: Object) extends VCObjectPrototype(underlying) with Product1[Object] {

  final def _1: Object = underlying

  override final def hashCode(): Int = {
    underlying.hashCode()
  }

  override final def toString: String = {
    s"$productPrefix($underlying)"
  }

  // subclasses are expected to implement equals, productPrefix, and canEqual
}

abstract class VCObjectCompanion[T <: VCObjectPrototype] extends ClassTag[T] {
  def box(underlying: Object): T
  final def unbox(boxed: T) = boxed.underlying
}

final class VCArrayObject[T <: VCObjectPrototype](val ct: VCObjectCompanion[T], sz: Int) extends VCArrayPrototype[T] {
  val arr = new Array[Object](sz)
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
