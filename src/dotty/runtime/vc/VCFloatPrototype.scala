package dotty.runtime.vc

import scala.reflect.ClassTag

import scala.runtime.Statics

abstract class VCFloatPrototype(val underlying: Float) extends VCPrototype {}

abstract class VCFloatCasePrototype(underlying: Float) extends VCFloatPrototype(underlying) with Product1[Float] {

  final def _1: Float = underlying

  override final def hashCode(): Int = {
    underlying.hashCode()
  }

  override final def toString: String = {
    s"$productPrefix($underlying)"
  }

  // subclasses are expected to implement equals, productPrefix, and canEqual
}

abstract class VCFloatCompanion[T <: VCFloatPrototype] extends ClassTag[T] {
  def box(underlying: Float): T
  final def unbox(boxed: T) = boxed.underlying
  override def newArray(len: Int): Array[T] =
    new VCFloatArray(this, len).asInstanceOf[Array[T]]


  final def _1$extension(underlying: Float)       = underlying
  final def hashCode$extension(underlying: Float) = underlying.hashCode()
  final def toString$extension(underlying: Float) = s"${productPrefix$extension(underlying)}($underlying)"
  def productPrefix$extension(underlying: Float): String
}

final class VCFloatArray[T <: VCFloatPrototype] private (val arr: Array[Float], val ct: VCFloatCompanion[T])
  extends VCArrayPrototype[T] {
  def this(ct: VCFloatCompanion[T], sz: Int) =
    this(new Array[Float](sz), ct)

  def apply(idx: Int) =
    ct.box(arr(idx))
  def update(idx: Int, elem: T) =
    arr(idx) = ct.unbox(elem)
  def length: Int = arr.length

  override def clone(): VCFloatArray[T] = {
    new VCFloatArray[T](arr.clone(), ct)
  }

  override def toString: String = {
    "[" + ct.runtimeClass
  }

  // Todo: what was the reason for 255 classes in my original proposal? arr.toString!
  // todo: need to discuss do we want to be compatible with ugly format of jvm here?
}
