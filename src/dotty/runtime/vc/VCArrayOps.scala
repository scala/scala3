package dotty.runtime.vc

import scala.collection.mutable.{WrappedArray, ArrayBuilder, ArrayOps}
import scala.reflect.ClassTag
import dotty.DottyPredef._

class VCArrayOps[T](xs: Array[T]) extends ArrayOps[T] {
  val array = xs
  def length: Int = array.length
  def apply(index: Int): T = array(index)
  def update(index: Int, elem: T) = {
    array(index) = elem
  }

  lazy val elemTag = (xs.asInstanceOf[Object]: @unchecked) match {
    case vc: VCIntArray[_] => vc.ct.asInstanceOf[ClassTag[T]]
    case vc: VCShortArray[_] => vc.ct.asInstanceOf[ClassTag[T]]
    case vc: VCLongArray[_] => vc.ct.asInstanceOf[ClassTag[T]]
    case vc: VCFloatArray[_] => vc.ct.asInstanceOf[ClassTag[T]]
    case vc: VCDoubleArray[_] => vc.ct.asInstanceOf[ClassTag[T]]
    case vc: VCByteArray[_] => vc.ct.asInstanceOf[ClassTag[T]]
    case vc: VCBooleanArray[_] => vc.ct.asInstanceOf[ClassTag[T]]
    case vc: VCCharArray[_] => vc.ct.asInstanceOf[ClassTag[T]]
    case vc: VCObjectArray[_] => vc.ct.asInstanceOf[ClassTag[T]]
  }

  override def repr = xs

  override protected[this] def newBuilder = new VCArrayBuilder[T]()(elemTag).asInstanceOf[ArrayBuilder[T]]
  //override protected[this] def newBuilder = new ArrayBuilder.ofRef[T]()(ClassTag[T](arrayElementClass(repr.getClass)))

  override protected[this] def thisCollection: WrappedArray[T] = wrapVCArray(xs)
  override protected[this] def toCollection(repr: Array[T]): WrappedArray[T] = wrapVCArray(xs)
}