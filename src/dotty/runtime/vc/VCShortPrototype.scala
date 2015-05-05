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

  implicit def classTag: this.type = this
  override def newArray(len: Int): Array[T] =
    new VCShortArray(this, len).asInstanceOf[Array[T]]


  final def _1$extension(underlying: Short)       = underlying
  final def hashCode$extension(underlying: Short) = underlying.hashCode()
  final def toString$extension(underlying: Short) = s"${productPrefix$extension(underlying)}($underlying)"
  def productPrefix$extension(underlying: Short): String
}

final class VCShortArray[T <: VCShortPrototype] private (val arr: Array[Short], val ct: VCShortCompanion[T])
  extends VCArrayPrototype[T] {
  def this(ct: VCShortCompanion[T], sz: Int) =
    this(new Array[Short](sz), ct)

  def apply(idx: Int) =
    ct.box(arr(idx))
  def update(idx: Int, elem: T) =
    arr(idx) = ct.unbox(elem)
  def length: Int = arr.length

  override def clone(): VCShortArray[T] = {
    new VCShortArray[T](arr.clone(), ct)
  }

  override def toString: String = {
    "[" + ct.runtimeClass
  }

  // Todo: what was the reason for 255 classes in my original proposal? arr.toString!
  // todo: need to discuss do we want to be compatible with ugly format of jvm here?
}
