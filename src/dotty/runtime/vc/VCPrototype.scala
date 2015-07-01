package dotty.runtime.vc

import scala.reflect.ClassTag

abstract class VCPrototype {
}


abstract class VCArrayPrototype[T <: VCPrototype] extends Object with Cloneable {
  def apply(idx: Int): Object
  def update(idx: Int, el: T): Unit
  def length: Int
  override def clone: Object = super.clone()
}


abstract class VCFloatPrototype(val underlying: Float) extends VCPrototype {}

abstract class VCFloatCasePrototype(underlying: Float) extends VCFloatPrototype(underlying) with Product1[Float] {

  final def _1: Float = underlying

  override final def hashCode(): Int = {
    underlying.hashCode()
  }

  override final def toString: String = {
    s"$productPrefix($underlying)"
  }
}

// NOTE for all VCXCompanion: The type parameter T should be bounded like this:
//   abstract class VCXCompanion[T <: VCXPrototype] extends ClassTag[T]
// But this affects erasure: it means that Array[T] is erased to [VCIntPrototype;
// instead of Object, but we really need it to erase to Object if we want
// VCXArray to be a valid array. We work around this by adding casts where
// we need to assume that T is a subtype of VCXPrototype.

abstract class VCFloatCompanion[T /*<: VCFloatPrototype*/] extends ClassTag[T] {
  def box(underlying: Float): T
  final def unbox(boxed: T) = boxed.asInstanceOf[VCFloatPrototype].underlying

  implicit def classTag: this.type = this
  override def newArray(len: Int): Array[T] =
    new VCFloatArray(this.asInstanceOf[VCFloatCompanion[VCFloatPrototype]], len).asInstanceOf[Array[T]]


  final def _1$extension(underlying: Float)       = underlying
  final def hashCode$extension(underlying: Float) = underlying.hashCode()
  def toString$extension(underlying: Float) = s"${productPrefix$extension(underlying)}($underlying)"
  def productPrefix$extension(underlying: Float): String
}

final class VCFloatArray[T <: VCFloatPrototype] (val arr: Array[Float], val ct: VCFloatCompanion[T])
  extends VCArrayPrototype[T] {
  def this(ct: VCFloatCompanion[T], sz: Int) =
    this(new Array[Float](sz), ct)

  def apply(idx: Int) =
    ct.box(arr(idx))
  def update(idx: Int, elem: T) =
    arr(idx) = ct.unbox(elem)
  def length: Int = arr.length

  override def clone(): VCFloatArray[T] = {
    new VCFloatArray[T](arr.clone(), ct)
  }

  override def toString: String = {
    "[" + ct.toString
  }
}


abstract class VCObjectPrototype(val underlying: Object) extends VCPrototype {}

abstract class VCObjectCasePrototype(underlying: Object) extends VCObjectPrototype(underlying) with Product1[Object] {

  final def _1: Object = underlying

  override final def hashCode(): Int = {
    underlying.hashCode()
  }

  override def toString: String = {
    s"$productPrefix($underlying)"
  }
}

abstract class VCObjectCompanion[T /*<: VCObjectPrototype*/] extends ClassTag[T] {
  def box(underlying: Object): T
  final def unbox(boxed: T) = boxed.asInstanceOf[VCObjectPrototype].underlying

  implicit def classTag: this.type = this
  override def newArray(len: Int): Array[T] =
    new VCObjectArray(this.asInstanceOf[VCObjectCompanion[VCObjectPrototype]], len).asInstanceOf[Array[T]]


  final def _1$extension(underlying: Object)       = underlying
  final def hashCode$extension(underlying: Object) = underlying.hashCode()
  def toString$extension(underlying: Object) = s"${productPrefix$extension(underlying)}($underlying)"
  def productPrefix$extension(underlying: Object): String
}

final class VCObjectArray[T <: VCObjectPrototype] (val arr: Array[Object], val ct: VCObjectCompanion[T])
  extends VCArrayPrototype[T] {
  def this(ct: VCObjectCompanion[T], sz: Int) =
    this(new Array[Object](sz), ct)

  def apply(idx: Int) =
    ct.box(arr(idx))

  def update(idx: Int, elem: T) =
    arr(idx) = ct.unbox(elem)

  def length: Int = arr.length

  override def clone(): VCObjectArray[T] = {
    new VCObjectArray[T](arr.clone(), ct)
  }

  override def toString: String = {
    "[" + ct.toString
  }
}


abstract class VCShortPrototype(val underlying: Short) extends VCPrototype {}

abstract class VCShortCasePrototype(underlying: Short) extends VCShortPrototype(underlying) with Product1[Short] {

  final def _1: Short = underlying

  override final def hashCode(): Int = {
    underlying.hashCode()
  }

  override def toString: String = {
    s"$productPrefix($underlying)"
  }
}

abstract class VCShortCompanion[T /*<: VCShortPrototype*/] extends ClassTag[T] {
  def box(underlying: Short): T
  final def unbox(boxed: T) = boxed.asInstanceOf[VCShortPrototype].underlying

  implicit def classTag: this.type = this
  override def newArray(len: Int): Array[T] =
    new VCShortArray(this.asInstanceOf[VCShortCompanion[VCShortPrototype]], len).asInstanceOf[Array[T]]


  final def _1$extension(underlying: Short)       = underlying
  final def hashCode$extension(underlying: Short) = underlying.hashCode()
  def toString$extension(underlying: Short) = s"${productPrefix$extension(underlying)}($underlying)"
  def productPrefix$extension(underlying: Short): String
}

final class VCShortArray[T <: VCShortPrototype] (val arr: Array[Short], val ct: VCShortCompanion[T])
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
    "[" + ct.toString
  }

}


abstract class VCLongPrototype(val underlying: Long) extends VCPrototype {}

abstract class VCLongCasePrototype(underlying: Long) extends VCLongPrototype(underlying) with Product1[Long] {

  final def _1: Long = underlying

  override final def hashCode(): Int = {
    underlying.hashCode()
  }

  override def toString: String = {
    s"$productPrefix($underlying)"
  }
}

abstract class VCLongCompanion[T /*<: VCLongPrototype*/] extends ClassTag[T] {
  def box(underlying: Long): T
  final def unbox(boxed: T) = boxed.asInstanceOf[VCLongPrototype].underlying

  implicit def classTag: this.type = this
  override def newArray(len: Int): Array[T] =
    new VCLongArray(this.asInstanceOf[VCLongCompanion[VCLongPrototype]], len).asInstanceOf[Array[T]]


  final def _1$extension(underlying: Long)       = underlying
  final def hashCode$extension(underlying: Long) = underlying.hashCode()
  def toString$extension(underlying: Long) = s"${productPrefix$extension(underlying)}($underlying)"
  def productPrefix$extension(underlying: Long): String
}

final class VCLongArray[T <: VCLongPrototype] (val arr: Array[Long], val ct: VCLongCompanion[T])
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
    "[" + ct.toString
  }
}


abstract class VCIntPrototype(val underlying: Int) extends VCPrototype {}

abstract class VCIntCasePrototype(underlying: Int) extends VCIntPrototype(underlying) with Product1[Int] {

  final def _1: Int = underlying

  override final def hashCode(): Int = {
    underlying.hashCode()
  }

  override def toString: String = {
    s"$productPrefix($underlying)"
  }
}

abstract class VCIntCompanion[T /*<: VCIntPrototype*/] extends ClassTag[T] {
  def box(underlying: Int): T
  final def unbox(boxed: T) = boxed.asInstanceOf[VCIntPrototype].underlying

  implicit def classTag: this.type = this
  override def newArray(len: Int): Array[T] =
    new VCIntArray(this.asInstanceOf[VCIntCompanion[VCIntPrototype]], len).asInstanceOf[Array[T]]


  final def _1$extension(underlying: Int)       = underlying
  final def hashCode$extension(underlying: Int) = underlying.hashCode()
  def toString$extension(underlying: Int) = s"${productPrefix$extension(underlying)}($underlying)"
  def productPrefix$extension(underlying: Int): String
}

final class VCIntArray[T <: VCIntPrototype] (val arr: Array[Int], val ct: VCIntCompanion[T])
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
    "[" + ct.toString
  }
}


abstract class VCDoublePrototype(val underlying: Double) extends VCPrototype {}

abstract class VCDoubleCasePrototype(underlying: Double) extends VCDoublePrototype(underlying) with Product1[Double] {

  final def _1: Double = underlying

  override final def hashCode(): Int = {
    underlying.hashCode()
  }

  override def toString: String = {
    s"$productPrefix($underlying)"
  }
}

abstract class VCDoubleCompanion[T /*<: VCDoublePrototype*/] extends ClassTag[T] {
  def box(underlying: Double): T
  final def unbox(boxed: T) = boxed.asInstanceOf[VCDoublePrototype].underlying

  implicit def classTag: this.type = this
  override def newArray(len: Int): Array[T] =
    new VCDoubleArray(this.asInstanceOf[VCDoubleCompanion[VCDoublePrototype]], len).asInstanceOf[Array[T]]


  final def _1$extension(underlying: Double)       = underlying
  final def hashCode$extension(underlying: Double) = underlying.hashCode()
  def toString$extension(underlying: Double) = s"${productPrefix$extension(underlying)}($underlying)"
  def productPrefix$extension(underlying: Double): String
}

final class VCDoubleArray[T <: VCDoublePrototype] (val arr: Array[Double], val ct: VCDoubleCompanion[T])
  extends VCArrayPrototype[T] {
  def this(ct: VCDoubleCompanion[T], sz: Int) =
    this(new Array[Double](sz), ct)

  def apply(idx: Int) =
    ct.box(arr(idx))
  def update(idx: Int, elem: T) =
    arr(idx) = ct.unbox(elem)
  def length: Int = arr.length

  override def clone(): VCDoubleArray[T] = {
    new VCDoubleArray[T](arr.clone(), ct)
  }

  override def toString: String = {
    "[" + ct.toString
  }
}


abstract class VCBooleanPrototype(val underlying: Boolean) extends VCPrototype {}

abstract class VCBooleanCasePrototype(underlying: Boolean) extends VCBooleanPrototype(underlying) with Product1[Boolean] {

  final def _1: Boolean = underlying

  override final def hashCode(): Int = {
    underlying.hashCode()
  }

  override def toString: String = {
    s"$productPrefix($underlying)"
  }
}

abstract class VCBooleanCompanion[T /*<: VCBooleanPrototype*/] extends ClassTag[T] {
  def box(underlying: Boolean): T
  final def unbox(boxed: T) = boxed.asInstanceOf[VCBooleanPrototype].underlying

  implicit def classTag: this.type = this
  override def newArray(len: Int): Array[T] =
    new VCBooleanArray(this.asInstanceOf[VCBooleanCompanion[VCBooleanPrototype]], len).asInstanceOf[Array[T]]


  final def _1$extension(underlying: Boolean)       = underlying
  final def hashCode$extension(underlying: Boolean) = underlying.hashCode()
  def toString$extension(underlying: Boolean) = s"${productPrefix$extension(underlying)}($underlying)"
  def productPrefix$extension(underlying: Boolean): String
}

final class VCBooleanArray[T <: VCBooleanPrototype] (val arr: Array[Boolean], val ct: VCBooleanCompanion[T])
  extends VCArrayPrototype[T] {
  def this(ct: VCBooleanCompanion[T], sz: Int) =
    this(new Array[Boolean](sz), ct)

  def apply(idx: Int) =
    ct.box(arr(idx))

  def update(idx: Int, elem: T) =
    arr(idx) = ct.unbox(elem)

  def length: Int = arr.length

  override def clone(): VCBooleanArray[T] = {
    new VCBooleanArray[T](arr.clone(), ct)
  }

  override def toString: String = {
    "[" + ct.toString
  }
}


abstract class VCCharPrototype(val underlying: Char) extends VCPrototype {}

abstract class VCCharCasePrototype(underlying: Char) extends VCCharPrototype(underlying) with Product1[Char] {

  final def _1: Char = underlying

  override final def hashCode(): Int = {
    underlying.hashCode()
  }

  override def toString: String = {
    s"$productPrefix($underlying)"
  }

  // subclasses are expected to implement equals, productPrefix, and canEqual
}

abstract class VCCharCompanion[T /*<: VCCharPrototype*/] extends ClassTag[T] {
  def box(underlying: Char): T
  final def unbox(boxed: T) = boxed.asInstanceOf[VCCharPrototype].underlying

  implicit def classTag: this.type = this
  override def newArray(len: Int): Array[T] =
    new VCCharArray(this.asInstanceOf[VCCharCompanion[VCCharPrototype]], len).asInstanceOf[Array[T]]


  final def _1$extension(underlying: Char)       = underlying
  final def hashCode$extension(underlying: Char) = underlying.hashCode()
  def toString$extension(underlying: Char) = s"${productPrefix$extension(underlying)}($underlying)"
  def productPrefix$extension(underlying: Char): String
}

final class VCCharArray[T <: VCCharPrototype] (val arr: Array[Char], val ct: VCCharCompanion[T])
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
    "[" + ct.toString
  }
}


abstract class VCBytePrototype(val underlying: Byte) extends VCPrototype {}

abstract class VCByteCasePrototype(underlying: Byte) extends VCBytePrototype(underlying) with Product1[Byte] {

  final def _1: Byte = underlying

  override final def hashCode(): Int = {
    underlying.hashCode()
  }

  override def toString: String = {
    s"$productPrefix($underlying)"
  }
}

abstract class VCByteCompanion[T /*<: VCBytePrototype*/] extends ClassTag[T] {
  def box(underlying: Byte): T
  final def unbox(boxed: T) = boxed.asInstanceOf[VCBytePrototype].underlying

  implicit def classTag: this.type = this
  override def newArray(len: Int): Array[T] =
    new VCByteArray(this.asInstanceOf[VCByteCompanion[VCBytePrototype]], len).asInstanceOf[Array[T]]


  final def _1$extension(underlying: Byte)       = underlying
  final def hashCode$extension(underlying: Byte) = underlying.hashCode()
  def toString$extension(underlying: Byte) = s"${productPrefix$extension(underlying)}($underlying)"
  def productPrefix$extension(underlying: Byte): String
}

final class VCByteArray[T <: VCBytePrototype] (val arr: Array[Byte], val ct: VCByteCompanion[T])
  extends VCArrayPrototype[T] {
  def this(ct: VCByteCompanion[T], sz: Int) =
    this(new Array[Byte](sz), ct)

  def apply(idx: Int) =
    ct.box(arr(idx))
  def update(idx: Int, elem: T) =
    arr(idx) = ct.unbox(elem)
  def length: Int = arr.length

  override def clone(): VCByteArray[T] = {
    new VCByteArray[T](arr.clone(), ct)
  }

  override def toString: String = {
    "[" + ct.toString
  }

}

