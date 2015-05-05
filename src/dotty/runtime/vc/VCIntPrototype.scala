package dotty.runtime.vc

import scala.reflect.ClassTag

import scala.runtime.Statics

abstract class VCIntPrototype(val underlying: Int) extends VCPrototype {}

abstract class VCIntCasePrototype(underlying: Int) extends VCIntPrototype(underlying) with Product1[Int] {

  final def _1: Int = underlying

  override final def hashCode(): Int = {
    underlying.hashCode()
  }

  override final def toString: String = {
    s"$productPrefix($underlying)"
  }

  // subclasses are expected to implement equals, productPrefix, and canEqual
}

abstract class VCIntCompanion[T <: VCIntPrototype] extends ClassTag[T] {
  def box(underlying: Int): T
  final def unbox(boxed: T) = boxed.underlying

  implicit def classTag: this.type = this
  override def newArray(len: Int): Array[T] =
    new VCIntArray(this, len).asInstanceOf[Array[T]]


  final def _1$extension(underlying: Int)       = underlying
  final def hashCode$extension(underlying: Int) = underlying.hashCode()
  final def toString$extension(underlying: Int) = s"${productPrefix$extension(underlying)}($underlying)"
  def productPrefix$extension(underlying: Int): String
}

final class VCIntArray[T <: VCIntPrototype] private (val arr: Array[Int], val ct: VCIntCompanion[T])
  extends VCArrayPrototype[T] {
  def this(ct: VCIntCompanion[T], sz: Int) =
    this(new Array[Int](sz), ct)

  def apply(idx: Int) =
    ct.box(arr(idx))
  def update(idx: Int, elem: T) =
    arr(idx) = ct.unbox(elem)
  def length: Int = arr.length

  override def clone(): VCIntArray[T] = {
    new VCIntArray[T](arr.clone(), ct)
  }

  override def toString: String = {
    "[" + ct.runtimeClass
  }

  // Todo: what was the reason for 255 classes in my original proposal? arr.toString!
  // todo: need to discuss do we want to be compatible with ugly format of jvm here?
}
