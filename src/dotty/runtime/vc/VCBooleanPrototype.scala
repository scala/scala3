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
  override def newArray(len: Int): Array[T] =
    new VCBooleanArray(this, len).asInstanceOf[Array[T]]


  final def _1$extension(underlying: Boolean)       = underlying
  final def hashCode$extension(underlying: Boolean) = underlying.hashCode()
  final def toString$extension(underlying: Boolean) = s"${productPrefix$extension(underlying)}($underlying)"
  def productPrefix$extension(underlying: Boolean): String
}

final class VCBooleanArray[T <: VCBooleanPrototype](val ct: VCBooleanCompanion[T], sz: Int) extends VCArrayPrototype[T] {
  var arr = new Array[Boolean](sz) // mutable for clone()
  def apply(idx: Int) =
    ct.box(arr(idx))
  def update(idx: Int, elem: T) =
    arr(idx) = ct.unbox(elem)
  def length: Int = arr.length

  override def clone(): VCBooleanArray[T] = {
    val t = super.clone().asInstanceOf[VCBooleanArray[T]]
    t.arr = this.arr.clone()
    t
  }

  override def toString: String = {
    "[" + ct.runtimeClass
  }

  // Todo: what was the reason for 255 classes in my original proposal? arr.toString!
  // todo: need to discuss do we want to be compatible with ugly format of jvm here?
}
