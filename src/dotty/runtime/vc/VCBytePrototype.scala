package dotty.runtime.vc

import scala.reflect.ClassTag

import scala.runtime.Statics

abstract class VCBytePrototype(val underlying: Byte) extends VCPrototype {}

abstract class VCByteCasePrototype(underlying: Byte) extends VCBytePrototype(underlying) with Product1[Byte] {

  final def _1: Byte = underlying

  override final def hashCode(): Int = {
    underlying.hashCode()
  }

  override final def toString: String = {
    s"$productPrefix($underlying)"
  }

  // subclasses are expected to implement equals, productPrefix, and canEqual
}

abstract class VCByteCompanion[T <: VCBytePrototype] extends ClassTag[T] {
  def box(underlying: Byte): T
  final def unbox(boxed: T) = boxed.underlying
  override def newArray(len: Int): Array[T] =
    new VCByteArray(this, len).asInstanceOf[Array[T]]

  final def _1$extension(underlying: Byte)       = underlying
  final def hashCode$extension(underlying: Byte) = underlying.hashCode()
  final def toString$extension(underlying: Byte) = s"${productPrefix$extension(underlying)}($underlying)"
  def productPrefix$extension(underlying: Byte): String
}

final class VCByteArray[T <: VCBytePrototype](val ct: VCByteCompanion[T], sz: Int) extends VCArrayPrototype[T] {
  var arr = new Array[Byte](sz) // mutable for clone
  def apply(idx: Int) =
    ct.box(arr(idx))
  def update(idx: Int, elem: T) =
    arr(idx) = ct.unbox(elem)
  def length: Int = arr.length

  override def clone(): VCByteArray[T] = {
    val t = super.clone().asInstanceOf[VCByteArray[T]]
    t.arr = this.arr.clone()
    t
  }


  override def toString: String = {
    "[" + ct.runtimeClass
  }

  // Todo: what was the reason for 255 classes in my original proposal? arr.toString!
  // todo: need to discuss do we want to be compatible with ugly format of jvm here?
}
