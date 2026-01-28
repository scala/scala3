package scala.runtime

import scala.collection.immutable.ArraySeq
import scala.reflect.ClassTag

sealed abstract class VarArgsBuilder[T]:
  def add(elem: T): this.type
  def addSeq(elems: Seq[T]): this.type
  def addArray(elems: Array[T]): this.type
  def result(): Seq[T]

object VarArgsBuilder:

  class generic[T](n: Int) extends VarArgsBuilder[T]:
    private val xs = new Array[AnyRef](n)
    override def result(): Seq[T] = ArraySeq.ofRef(xs).asInstanceOf[ArraySeq[T]]
    private var i = 0
    override def add(elem: T): this.type =
      xs(i) = elem.asInstanceOf[AnyRef]
      i += 1
      this
    override def addSeq(elems: Seq[T]): this.type =
      for elem <- elems do
        xs(i) = elem.asInstanceOf[AnyRef]
        i += 1
      this
    override def addArray(elems: Array[T]): this.type =
      for elem <- elems do
        xs(i) = elem.asInstanceOf[AnyRef]
        i += 1
      this

  class ofRef[T <: AnyRef](n: Int) extends VarArgsBuilder[T]:
    private val xs = new Array[AnyRef](n)
    override def result(): Seq[T] = ArraySeq.ofRef(xs).asInstanceOf[ArraySeq[T]]
    private var i = 0
    override def add(elem: T): this.type =
      xs(i) = elem
      i += 1
      this
    override def addSeq(elems: Seq[T]): this.type =
      for elem <- elems do
        xs(i) = elem
        i += 1
      this
    override def addArray(elems: Array[T]): this.type =
      for elem <- elems do
        xs(i) = elem
        i += 1
      this

  class ofByte(n: Int) extends VarArgsBuilder[Byte]:
    private val xs = new Array[Byte](n)
    override def result(): Seq[Byte] = ArraySeq.ofByte(xs)
    private var i = 0
    override def add(elem: Byte): this.type =
      xs(i) = elem
      i += 1
      this
    override def addSeq(elems: Seq[Byte]): this.type =
      for elem <- elems do
        xs(i) = elem
        i += 1
      this
    override def addArray(elems: Array[Byte]): this.type =
      for elem <- elems do
        xs(i) = elem
        i += 1
      this

  class ofShort(n: Int) extends VarArgsBuilder[Short]:
    private val xs = new Array[Short](n)
    override def result(): Seq[Short] = ArraySeq.ofShort(xs)
    private var i = 0
    override def add(elem: Short): this.type =
      xs(i) = elem
      i += 1
      this
    override def addSeq(elems: Seq[Short]): this.type =
      for elem <- elems do
        xs(i) = elem
        i += 1
      this
    override def addArray(elems: Array[Short]): this.type =
      for elem <- elems do
        xs(i) = elem
        i += 1
      this

  class ofChar(n: Int) extends VarArgsBuilder[Char]:
    private val xs = new Array[Char](n)
    override def result(): Seq[Char] = ArraySeq.ofChar(xs)
    private var i = 0
    override def add(elem: Char): this.type =
      xs(i) = elem
      i += 1
      this
    override def addSeq(elems: Seq[Char]): this.type =
      for elem <- elems do
        xs(i) = elem
        i += 1
      this
    override def addArray(elems: Array[Char]): this.type =
      for elem <- elems do
        xs(i) = elem
        i += 1
      this

  class ofInt(n: Int) extends VarArgsBuilder[Int]:
    private val xs = new Array[Int](n)
    override def result(): Seq[Int] = ArraySeq.ofInt(xs)
    private var i = 0
    override def add(elem: Int): this.type =
      xs(i) = elem
      i += 1
      this
    override def addSeq(elems: Seq[Int]): this.type =
      for elem <- elems do
        xs(i) = elem
        i += 1
      this
    override def addArray(elems: Array[Int]): this.type =
      for elem <- elems do
        xs(i) = elem
        i += 1
      this

  class ofLong(n: Int) extends VarArgsBuilder[Long]:
    private val xs = new Array[Long](n)
    override def result(): Seq[Long] = ArraySeq.ofLong(xs)
    private var i = 0
    override def add(elem: Long): this.type =
      xs(i) = elem
      i += 1
      this
    override def addSeq(elems: Seq[Long]): this.type =
      for elem <- elems do
        xs(i) = elem
        i += 1
      this
    override def addArray(elems: Array[Long]): this.type =
      for elem <- elems do
        xs(i) = elem
        i += 1
      this

  class ofFloat(n: Int) extends VarArgsBuilder[Float]:
    private val xs = new Array[Float](n)
    override def result(): Seq[Float] = ArraySeq.ofFloat(xs)
    private var i = 0
    override def add(elem: Float): this.type =
      xs(i) = elem
      i += 1
      this
    override def addSeq(elems: Seq[Float]): this.type =
      for elem <- elems do
        xs(i) = elem
        i += 1
      this
    override def addArray(elems: Array[Float]): this.type =
      for elem <- elems do
        xs(i) = elem
        i += 1
      this

  class ofDouble(n: Int) extends VarArgsBuilder[Double]:
    private val xs = new Array[Double](n)
    override def result(): Seq[Double] = ArraySeq.ofDouble(xs)
    private var i = 0
    override def add(elem: Double): this.type =
      xs(i) = elem
      i += 1
      this
    override def addSeq(elems: Seq[Double]): this.type =
      for elem <- elems do
        xs(i) = elem
        i += 1
      this
    override def addArray(elems: Array[Double]): this.type =
      for elem <- elems do
        xs(i) = elem
        i += 1
      this

  class ofBoolean(n: Int) extends VarArgsBuilder[Boolean]:
    private val xs = new Array[Boolean](n)
    override def result(): Seq[Boolean] = ArraySeq.ofBoolean(xs)
    private var i = 0
    override def add(elem: Boolean): this.type =
      xs(i) = elem
      i += 1
      this
    override def addSeq(elems: Seq[Boolean]): this.type =
      for elem <- elems do
        xs(i) = elem
        i += 1
      this
    override def addArray(elems: Array[Boolean]): this.type =
      for elem <- elems do
        xs(i) = elem
        i += 1
      this

  class ofUnit(n: Int) extends VarArgsBuilder[Unit]:
    private val xs = new Array[Unit](n)
    override def result(): Seq[Unit] = ArraySeq.ofUnit(xs)
    private var i = 0
    override def add(elem: Unit): this.type =
      xs(i) = elem
      i += 1
      this
    override def addSeq(elems: Seq[Unit]): this.type =
      for elem <- elems do
        xs(i) = elem
        i += 1
      this
    override def addArray(elems: Array[Unit]): this.type =
      for elem <- elems do
        xs(i) = elem
        i += 1
      this

end VarArgsBuilder
