package scala.collection.mutable

import scala.reflect.ClassTag

/** A builder class for immutable arrays.
 *
 *  @tparam T    the type of the elements for the builder.
 */
@SerialVersionUID(3L)
sealed abstract class IArrayBuilder[T]
  extends ReusableBuilder[T, IArray[T]]
    with Serializable {

  protected[this] val arrayBuilder: ArrayBuilder[T]

  override final def sizeHint(size: Int): Unit = arrayBuilder.sizeHint(size)
  def clear(): Unit = arrayBuilder.clear()

  /** Add all elements of an array */
  def addAll(xs: IArray[T] | Array[_ <: T]): this.type =
    arrayBuilder.addAll(xs.asInstanceOf[Array[T]])
    this

  /** Add a slice of an array */
  def addAll(xs: IArray[T] | Array[_ <: T], offset: Int, length: Int): this.type =
    arrayBuilder.addAll(xs.asInstanceOf[Array[T]], offset, length)
    this

  override def addAll(xs: IterableOnce[T]): this.type =
    arrayBuilder.addAll(xs)
    this
}

/** A companion object for array builders. */
object IArrayBuilder {

  /** Creates a new arraybuilder of type `T`.
  *
  *  @tparam T     type of the elements for the array builder, with a `ClassTag` context bound.
  *  @return       a new empty array builder.
  */
  @inline def make[T: ClassTag]: IArrayBuilder[T] = {
    val tag = implicitly[ClassTag[T]]
    tag.runtimeClass match {
      case java.lang.Byte.TYPE      => new IArrayBuilder.ofByte().asInstanceOf[IArrayBuilder[T]]
      case java.lang.Short.TYPE     => new IArrayBuilder.ofShort().asInstanceOf[IArrayBuilder[T]]
      case java.lang.Character.TYPE => new IArrayBuilder.ofChar().asInstanceOf[IArrayBuilder[T]]
      case java.lang.Integer.TYPE   => new IArrayBuilder.ofInt().asInstanceOf[IArrayBuilder[T]]
      case java.lang.Long.TYPE      => new IArrayBuilder.ofLong().asInstanceOf[IArrayBuilder[T]]
      case java.lang.Float.TYPE     => new IArrayBuilder.ofFloat().asInstanceOf[IArrayBuilder[T]]
      case java.lang.Double.TYPE    => new IArrayBuilder.ofDouble().asInstanceOf[IArrayBuilder[T]]
      case java.lang.Boolean.TYPE   => new IArrayBuilder.ofBoolean().asInstanceOf[IArrayBuilder[T]]
      case java.lang.Void.TYPE      => new IArrayBuilder.ofUnit().asInstanceOf[IArrayBuilder[T]]
      case _                        => new IArrayBuilder.ofRef[T with AnyRef]()(tag.asInstanceOf[ClassTag[T with AnyRef]]).asInstanceOf[IArrayBuilder[T]]
    }
  }

  /** A class for array builders for arrays of reference types.
  *
  *  This builder can be reused.
  *
  *  @tparam T     type of elements for the array builder, subtype of `AnyRef` with a `ClassTag` context bound.
  */
  @SerialVersionUID(3L)
  final class ofRef[T <: AnyRef](implicit ct: ClassTag[T]) extends IArrayBuilder[T] {
    protected[this] val arrayBuilder: ArrayBuilder[T] = new ArrayBuilder.ofRef
    def addOne(elem: T): this.type = { arrayBuilder.addOne(elem); this }
    def result(): IArray[T] = IArray.unsafeFromArray(arrayBuilder.result())
    override def equals(other: Any): Boolean = other match {
      case x: ofRef[_] => this.arrayBuilder == x.arrayBuilder
      case _ => false
    }
    override def toString = "IArrayBuilder.ofRef"
  }

  /** A class for array builders for arrays of `byte`s. It can be reused. */
  @SerialVersionUID(3L)
  final class ofByte extends IArrayBuilder[Byte] {
    protected[this] val arrayBuilder: ArrayBuilder[Byte] = new ArrayBuilder.ofByte
    def addOne(elem: Byte): this.type = { arrayBuilder.addOne(elem); this }
    def result(): IArray[Byte] = IArray.unsafeFromArray(arrayBuilder.result())
    override def equals(other: Any): Boolean = other match {
      case x: ofByte => this.arrayBuilder == x.arrayBuilder
      case _ => false
    }
    override def toString = "IArrayBuilder.ofByte"
  }

  /** A class for array builders for arrays of `short`s. It can be reused. */
  @SerialVersionUID(3L)
  final class ofShort extends IArrayBuilder[Short] {
    protected[this] val arrayBuilder: ArrayBuilder[Short] = new ArrayBuilder.ofShort
    def addOne(elem: Short): this.type = { arrayBuilder.addOne(elem); this }
    def result(): IArray[Short] = IArray.unsafeFromArray(arrayBuilder.result())
    override def equals(other: Any): Boolean = other match {
      case x: ofShort => this.arrayBuilder == x.arrayBuilder
      case _ => false
    }
    override def toString = "IArrayBuilder.ofShort"
  }

  /** A class for array builders for arrays of `char`s. It can be reused. */
  @SerialVersionUID(3L)
  final class ofChar extends IArrayBuilder[Char] {
    protected[this] val arrayBuilder: ArrayBuilder[Char] = new ArrayBuilder.ofChar
    def addOne(elem: Char): this.type = { arrayBuilder.addOne(elem); this }
    def result(): IArray[Char] = IArray.unsafeFromArray(arrayBuilder.result())
    override def equals(other: Any): Boolean = other match {
      case x: ofChar => this.arrayBuilder == x.arrayBuilder
      case _ => false
    }
    override def toString = "IArrayBuilder.ofChar"
  }

  /** A class for array builders for arrays of `int`s. It can be reused. */
  @SerialVersionUID(3L)
  final class ofInt extends IArrayBuilder[Int] {
    protected[this] val arrayBuilder: ArrayBuilder[Int] = new ArrayBuilder.ofInt
    def addOne(elem: Int): this.type = { arrayBuilder.addOne(elem); this }
    def result(): IArray[Int] = IArray.unsafeFromArray(arrayBuilder.result())
    override def equals(other: Any): Boolean = other match {
      case x: ofInt => this.arrayBuilder == x.arrayBuilder
      case _ => false
    }
    override def toString = "IArrayBuilder.ofInt"
  }

  /** A class for array builders for arrays of `long`s. It can be reused. */
  @SerialVersionUID(3L)
  final class ofLong extends IArrayBuilder[Long] {
    protected[this] val arrayBuilder: ArrayBuilder[Long] = new ArrayBuilder.ofLong
    def addOne(elem: Long): this.type = { arrayBuilder.addOne(elem); this }
    def result(): IArray[Long] = IArray.unsafeFromArray(arrayBuilder.result())
    override def equals(other: Any): Boolean = other match {
      case x: ofLong => this.arrayBuilder == x.arrayBuilder
      case _ => false
    }
    override def toString = "IArrayBuilder.ofLong"
  }

  /** A class for array builders for arrays of `float`s. It can be reused. */
  @SerialVersionUID(3L)
  final class ofFloat extends IArrayBuilder[Float] {
    protected[this] val arrayBuilder: ArrayBuilder[Float] = new ArrayBuilder.ofFloat
    def addOne(elem: Float): this.type = { arrayBuilder.addOne(elem); this }
    def result(): IArray[Float] = IArray.unsafeFromArray(arrayBuilder.result())
    override def equals(other: Any): Boolean = other match {
      case x: ofFloat => this.arrayBuilder == x.arrayBuilder
      case _ => false
    }
    override def toString = "IArrayBuilder.ofFloat"
  }

  /** A class for array builders for arrays of `double`s. It can be reused. */
  @SerialVersionUID(3L)
  final class ofDouble extends IArrayBuilder[Double] {
    protected[this] val arrayBuilder: ArrayBuilder[Double] = new ArrayBuilder.ofDouble
    def addOne(elem: Double): this.type = { arrayBuilder.addOne(elem); this }
    def result(): IArray[Double] = IArray.unsafeFromArray(arrayBuilder.result())
    override def equals(other: Any): Boolean = other match {
      case x: ofDouble => this.arrayBuilder == x.arrayBuilder
      case _ => false
    }
    override def toString = "IArrayBuilder.ofDouble"
  }

  /** A class for array builders for arrays of `boolean`s. It can be reused. */
  @SerialVersionUID(3L)
  class ofBoolean extends IArrayBuilder[Boolean] {
    protected[this] val arrayBuilder: ArrayBuilder[Boolean] = new ArrayBuilder.ofBoolean
    def addOne(elem: Boolean): this.type = { arrayBuilder.addOne(elem); this }
    def result(): IArray[Boolean] = IArray.unsafeFromArray(arrayBuilder.result())
    override def equals(other: Any): Boolean = other match {
      case x: ofBoolean => this.arrayBuilder == x.arrayBuilder
      case _ => false
    }
    override def toString = "IArrayBuilder.ofBoolean"
  }

  /** A class for array builders for arrays of `Unit` type. It can be reused. */
  @SerialVersionUID(3L)
  final class ofUnit extends IArrayBuilder[Unit] {
    protected[this] val arrayBuilder: ArrayBuilder[Unit] = new ArrayBuilder.ofUnit
    def addOne(elem: Unit): this.type = { arrayBuilder.addOne(elem); this }
    def result(): IArray[Unit] = IArray.unsafeFromArray(arrayBuilder.result())
    override def equals(other: Any): Boolean = other match {
      case x: ofUnit => this.arrayBuilder == x.arrayBuilder
      case _ => false
    }
    override def toString = "IArrayBuilder.ofUnit"
  }
}
