package dotty.runtime.vc

import scala.reflect.ClassTag

import scala.runtime.Statics

abstract class VCCharPrototype(val underlying: Char) extends VCPrototype {}

abstract class VCCharCasePrototype(underlying: Char) extends VCCharPrototype(underlying) with Product1[Char] {

  final def _1: Char = underlying

  override final def hashCode(): Int = {
    underlying.hashCode()
  }

  override final def toString: String = {
    s"$productPrefix($underlying)"
  }

  // subclasses are expected to implement equals, productPrefix, and canEqual
}

abstract class VCCharCompanion[T <: VCCharPrototype] extends ClassTag[T] {
  def box(underlying: Char): T
  final def unbox(boxed: T) = boxed.underlying
  override def newArray(len: Int): Array[T] =
    new VCCharArray(this, len).asInstanceOf[Array[T]]


  final def _1$extension(underlying: Char)       = underlying
  final def hashCode$extension(underlying: Char) = underlying.hashCode()
  final def toString$extension(underlying: Char) = s"${productPrefix$extension(underlying)}($underlying)"
  def productPrefix$extension(underlying: Char): String
}

final class VCCharArray[T <: VCCharPrototype] private (val arr: Array[Char], val ct: VCCharCompanion[T])
  extends VCArrayPrototype[T] {
  def this(ct: VCCharCompanion[T], sz: Int) =
    this(new Array[Char](sz), ct)

  def apply(idx: Int) =
    ct.box(arr(idx))
  def update(idx: Int, elem: T) =
    arr(idx) = ct.unbox(elem)
  def length: Int = arr.length

  override def clone(): VCCharArray[T] = {
    new VCCharArray[T](arr.clone(), ct)
  }

  override def toString: String = {
    "[" + ct.runtimeClass
  }

  // Todo: what was the reason for 255 classes in my original proposal? arr.toString!
  // todo: need to discuss do we want to be compatible with ugly format of jvm here?
}
