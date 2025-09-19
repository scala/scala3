package scala.runtime

import scala.collection.immutable.ArraySeq
import scala.reflect.ClassTag

sealed abstract class VarArgsBuilder[T]:
  def add(elem: T): this.type
  def addSeq(elems: Seq[T]): this.type
  def addArray(elems: Array[T]): this.type
  def result(): Seq[T]

object VarArgsBuilder:

  def generic[T](n: Int): VarArgsBuilder[T] = new VarArgsBuilder[T]:
    private val xs = new Array[AnyRef](n)
    def result() = ArraySeq.ofRef(xs).asInstanceOf[ArraySeq[T]]
    private var i = 0
    def add(elem: T): this.type =
      xs(i) = elem.asInstanceOf[AnyRef]
      i += 1
      this
    def addSeq(elems: Seq[T]): this.type =
      for elem <- elems do
        xs(i) = elem.asInstanceOf[AnyRef]
        i += 1
      this
    def addArray(elems: Array[T]): this.type =
      for elem <- elems do
        xs(i) = elem.asInstanceOf[AnyRef]
        i += 1
      this

  def ofRef[T <: AnyRef](n: Int): VarArgsBuilder[T] = new VarArgsBuilder[T]:
    private val xs = new Array[AnyRef](n)
    def result() = ArraySeq.ofRef(xs).asInstanceOf[ArraySeq[T]]
    private var i = 0
    def add(elem: T): this.type =
      xs(i) = elem
      i += 1
      this
    def addSeq(elems: Seq[T]): this.type =
      for elem <- elems do
        xs(i) = elem
        i += 1
      this
    def addArray(elems: Array[T]): this.type =
      for elem <- elems do
        xs(i) = elem
        i += 1
      this

  def ofByte(n: Int): VarArgsBuilder[Byte] = new VarArgsBuilder[Byte]:
    private val xs = new Array[Byte](n)
    def result() = ArraySeq.ofByte(xs)
    private var i = 0
    def add(elem: Byte): this.type =
      xs(i) = elem
      i += 1
      this
    def addSeq(elems: Seq[Byte]): this.type =
      for elem <- elems do
        xs(i) = elem
        i += 1
      this
    def addArray(elems: Array[Byte]): this.type =
      for elem <- elems do
        xs(i) = elem
        i += 1
      this

  def ofShort(n: Int): VarArgsBuilder[Short] = new VarArgsBuilder[Short]:
    private val xs = new Array[Short](n)
    def result() = ArraySeq.ofShort(xs)
    private var i = 0
    def add(elem: Short): this.type =
      xs(i) = elem
      i += 1
      this
    def addSeq(elems: Seq[Short]): this.type =
      for elem <- elems do
        xs(i) = elem
        i += 1
      this
    def addArray(elems: Array[Short]): this.type =
      for elem <- elems do
        xs(i) = elem
        i += 1
      this

  def ofChar(n: Int): VarArgsBuilder[Char] = new VarArgsBuilder[Char]:
    private val xs = new Array[Char](n)
    def result() = ArraySeq.ofChar(xs)
    private var i = 0
    def add(elem: Char): this.type =
      xs(i) = elem
      i += 1
      this
    def addSeq(elems: Seq[Char]): this.type =
      for elem <- elems do
        xs(i) = elem
        i += 1
      this
    def addArray(elems: Array[Char]): this.type =
      for elem <- elems do
        xs(i) = elem
        i += 1
      this

  def ofInt(n: Int): VarArgsBuilder[Int] = new VarArgsBuilder[Int]:
    private val xs = new Array[Int](n)
    def result() = ArraySeq.ofInt(xs)
    private var i = 0
    def add(elem: Int): this.type =
      xs(i) = elem
      i += 1
      this
    def addSeq(elems: Seq[Int]): this.type =
      for elem <- elems do
        xs(i) = elem
        i += 1
      this
    def addArray(elems: Array[Int]): this.type =
      for elem <- elems do
        xs(i) = elem
        i += 1
      this

  def ofLong(n: Int): VarArgsBuilder[Long] = new VarArgsBuilder[Long]:
    private val xs = new Array[Long](n)
    def result() = ArraySeq.ofLong(xs)
    private var i = 0
    def add(elem: Long): this.type =
      xs(i) = elem
      i += 1
      this
    def addSeq(elems: Seq[Long]): this.type =
      for elem <- elems do
        xs(i) = elem
        i += 1
      this
    def addArray(elems: Array[Long]): this.type =
      for elem <- elems do
        xs(i) = elem
        i += 1
      this

  def ofFloat(n: Int): VarArgsBuilder[Float] = new VarArgsBuilder[Float]:
    private val xs = new Array[Float](n)
    def result() = ArraySeq.ofFloat(xs)
    private var i = 0
    def add(elem: Float): this.type =
      xs(i) = elem
      i += 1
      this
    def addSeq(elems: Seq[Float]): this.type =
      for elem <- elems do
        xs(i) = elem
        i += 1
      this
    def addArray(elems: Array[Float]): this.type =
      for elem <- elems do
        xs(i) = elem
        i += 1
      this

  def ofDouble(n: Int): VarArgsBuilder[Double] = new VarArgsBuilder[Double]:
    private val xs = new Array[Double](n)
    def result() = ArraySeq.ofDouble(xs)
    private var i = 0
    def add(elem: Double): this.type =
      xs(i) = elem
      i += 1
      this
    def addSeq(elems: Seq[Double]): this.type =
      for elem <- elems do
        xs(i) = elem
        i += 1
      this
    def addArray(elems: Array[Double]): this.type =
      for elem <- elems do
        xs(i) = elem
        i += 1
      this

  def ofBoolean(n: Int): VarArgsBuilder[Boolean] = new VarArgsBuilder[Boolean]:
    private val xs = new Array[Boolean](n)
    def result() = ArraySeq.ofBoolean(xs)
    private var i = 0
    def add(elem: Boolean): this.type =
      xs(i) = elem
      i += 1
      this
    def addSeq(elems: Seq[Boolean]): this.type =
      for elem <- elems do
        xs(i) = elem
        i += 1
      this
    def addArray(elems: Array[Boolean]): this.type =
      for elem <- elems do
        xs(i) = elem
        i += 1
      this

  def ofUnit(n: Int): VarArgsBuilder[Unit] = new VarArgsBuilder[Unit]:
    private val xs = new Array[Unit](n)
    def result() = ArraySeq.ofUnit(xs)
    private var i = 0
    def add(elem: Unit): this.type =
      xs(i) = elem
      i += 1
      this
    def addSeq(elems: Seq[Unit]): this.type =
      for elem <- elems do
        xs(i) = elem
        i += 1
      this
    def addArray(elems: Array[Unit]): this.type =
      for elem <- elems do
        xs(i) = elem
        i += 1
      this

end VarArgsBuilder