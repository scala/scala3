package dotty.runtime.vc

import scala.collection.mutable.{Builder, WrappedArray, ArrayBuilder, ArrayOps}
import scala.reflect.ClassTag
import dotty.DottyPredef._

import scala.runtime.ScalaRunTime._

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

  final def vcElementClass: Class[_] = arrayElementClass(elemTag)

  override def toArray[U >: T : ClassTag]: Array[U] = {
    val thatElementClass = arrayElementClass(implicitly[ClassTag[U]])
    if (vcElementClass eq thatElementClass)
      repr.asInstanceOf[Array[U]]
    else
      super.toArray[U]
  }

  override def transpose[U](implicit asArray: T => Array[U]): Array[Array[U]] = {
    val bb: Builder[Array[U], Array[Array[U]]] = Array.newBuilder(ClassTag[Array[U]](vcElementClass))
    if (isEmpty) bb.result()
    else {
      def mkRowBuilder() = Array.newBuilder(ClassTag[U](arrayElementClass(vcElementClass)))
      val bs = asArray(head) map (_ => mkRowBuilder())
      for (xs <- this) {
        var i = 0
        for (x <- asArray(xs)) {
          bs(i) += x
          i += 1
        }
      }
      for (b <- bs) bb += b.result()
      bb.result()
    }
  }
}