package dotty.runtime.vc

import scala.reflect.ClassTag

import scala.runtime.Statics

abstract class VCDoublePrototype(val underlying: Double) extends VCPrototype {}

abstract class VCDoubleCasePrototype(underlying: Double) extends VCDoublePrototype(underlying) with Product1[Double] {

  final def _1: Double = underlying

  override final def hashCode(): Int = {
    underlying.hashCode()
  }

  override final def toString: String = {
    s"$productPrefix($underlying)"
  }

  // subclasses are expected to implement equals, productPrefix, and canEqual
}

abstract class VCDoubleCompanion[T <: VCDoublePrototype] extends ClassTag[T] {
  def box(underlying: Double): T
  final def unbox(boxed: T) = boxed.underlying

  final def _1$extension(underlying: Double)       = underlying
  final def hashCode$extension(underlying: Double) = underlying.hashCode()
  final def toString$extension(underlying: Double) = s"${productPrefix$extension(underlying)}($underlying)"
  def productPrefix$extension(underlying: Double): String
}

final class VCArrayDouble[T <: VCDoublePrototype](val ct: VCDoubleCompanion[T], sz: Int) extends VCArrayPrototype[T] {
  val arr = new Array[Double](sz)
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
