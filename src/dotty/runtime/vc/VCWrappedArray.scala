package dotty.runtime.vc

import scala.collection.mutable.WrappedArray
import scala.reflect.ClassTag

class VCWrappedArray[T](xs: Array[T]) extends WrappedArray[T] {
  val array = xs
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
  //lazy val elemTag = ClassTag[T](arrayElementClass(array.getClass))

  def length: Int = array.length
  def apply(index: Int): T = array(index)
  def update(index: Int, elem: T) = {
    array(index) = elem
  }
}