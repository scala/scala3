package dotty.runtime.vc

import scala.reflect.ClassTag

import scala.runtime.Statics

abstract class VCLongPrototype(val underlying: Long) extends VCPrototype {}

abstract class VCLongCasePrototype(underlying: Long) extends VCLongPrototype(underlying) with Product1[Long] {

  final def _1: Long = underlying

  override final def hashCode(): Int = {
    underlying.hashCode()
  }

  override final def toString: String = {
    s"$productPrefix($underlying)"
  }

  // subclasses are expected to implement equals, productPrefix, and canEqual
}

abstract class VCLongCompanion[T <: VCLongPrototype] extends ClassTag[T] {
  def box(underlying: Long): T
  final def unbox(boxed: T) = boxed.underlying
  override def newArray(len: Int): Array[T] =
    new VCLongArray(this, len).asInstanceOf[Array[T]]


  final def _1$extension(underlying: Long)       = underlying
  final def hashCode$extension(underlying: Long) = underlying.hashCode()
  final def toString$extension(underlying: Long) = s"${productPrefix$extension(underlying)}($underlying)"
  def productPrefix$extension(underlying: Long): String
}

final class VCLongArray[T <: VCLongPrototype] private (val arr: Array[Long], val ct: VCLongCompanion[T])
  extends VCArrayPrototype[T] {
  def this(ct: VCLongCompanion[T], sz: Int) =
    this(new Array[Long](sz), ct)

  def apply(idx: Int) =
    ct.box(arr(idx))
  def update(idx: Int, elem: T) =
    arr(idx) = ct.unbox(elem)
  def length: Int = arr.length

  override def clone(): VCLongArray[T] = {
    new VCLongArray[T](arr.clone(), ct)
  }

  override def toString: String = {
    "[" + ct.runtimeClass
  }

  // Todo: what was the reason for 255 classes in my original proposal? arr.toString!
  // todo: need to discuss do we want to be compatible with ugly format of jvm here?
}
