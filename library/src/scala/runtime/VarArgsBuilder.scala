package scala.runtime

import scala.collection.immutable.ArraySeq
import scala.reflect.ClassTag

/** A builder used by compiler-generated code to construct the sequences produced
 *  by sequence literals that contain spread operators.
 *
 *  The compiler (in `PostTyper`) translates a sequence literal such as
 *  `[1, xs*, 2, ys*]` into
 *  {{{
 *  scala.runtime.VarArgsBuilder.ofInt(2 + xs.length + ys.length)
 *    .add(1)
 *    .addSeq(xs)
 *    .add(2)
 *    .addSeq(ys)
 *    .result()
 *  }}}
 *  choosing the companion-object factory that matches the element type. Each
 *  factory receives the total number of elements up front, and the generated
 *  code adds exactly that many elements before calling `result()` once.
 *
 *  @tparam T the type of the elements of the sequence being built
 */
sealed abstract class VarArgsBuilder[T]:
  /** Adds a single element to the sequence being built.
   *
   *  The compiler emits one call to this method per non-spread element of the
   *  sequence literal.
   *
   *  @param elem the element to add
   *  @return this builder
   */
  def add(elem: T): this.type
  /** Adds every element of a sequence, in order, to the sequence being built.
   *
   *  The compiler emits a call to this method for each spread element `xs*`
   *  whose `xs` is a `Seq`.
   *
   *  @param elems the sequence of elements to add
   *  @return this builder
   */
  def addSeq(elems: Seq[T]): this.type
  /** Adds every element of an array, in order, to the sequence being built.
   *
   *  The compiler emits a call to this method for each spread element `xs*`
   *  whose `xs` is an array.
   *
   *  @param elems the array of elements to add
   *  @return this builder
   */
  def addArray(elems: Array[T]): this.type
  /** Returns a sequence of the elements that were added to this builder, in order. */
  def result(): Seq[T]

object VarArgsBuilder:

  /** Returns a builder for elements of a type not statically known to be a
   *  primitive or a reference type, such as an unbounded type parameter.
   *
   *  The builder stores each element, cast to `AnyRef`, in an `Array[AnyRef]`
   *  of length `n`; `result()` wraps that array in an `ArraySeq.ofRef` cast to
   *  `ArraySeq[T]`, without copying.
   *
   *  @tparam T the element type
   *  @param n the exact number of elements that will be added to the builder
   */
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

  /** Returns a builder for elements of a reference type.
   *
   *  The builder stores elements in an `Array[AnyRef]` of length `n`;
   *  `result()` wraps that array in an `ArraySeq.ofRef` cast to `ArraySeq[T]`,
   *  without copying.
   *
   *  @tparam T the element type, a reference type
   *  @param n the exact number of elements that will be added to the builder
   */
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

  /** Returns a builder for `Byte` elements.
   *
   *  The builder stores elements in an `Array[Byte]` of length `n`; `result()`
   *  wraps that array in an `ArraySeq.ofByte`, without copying.
   *
   *  @param n the exact number of elements that will be added to the builder
   */
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

  /** Returns a builder for `Short` elements.
   *
   *  The builder stores elements in an `Array[Short]` of length `n`; `result()`
   *  wraps that array in an `ArraySeq.ofShort`, without copying.
   *
   *  @param n the exact number of elements that will be added to the builder
   */
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

  /** Returns a builder for `Char` elements.
   *
   *  The builder stores elements in an `Array[Char]` of length `n`; `result()`
   *  wraps that array in an `ArraySeq.ofChar`, without copying.
   *
   *  @param n the exact number of elements that will be added to the builder
   */
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

  /** Returns a builder for `Int` elements.
   *
   *  The builder stores elements in an `Array[Int]` of length `n`; `result()`
   *  wraps that array in an `ArraySeq.ofInt`, without copying.
   *
   *  @param n the exact number of elements that will be added to the builder
   */
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

  /** Returns a builder for `Long` elements.
   *
   *  The builder stores elements in an `Array[Long]` of length `n`; `result()`
   *  wraps that array in an `ArraySeq.ofLong`, without copying.
   *
   *  @param n the exact number of elements that will be added to the builder
   */
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

  /** Returns a builder for `Float` elements.
   *
   *  The builder stores elements in an `Array[Float]` of length `n`; `result()`
   *  wraps that array in an `ArraySeq.ofFloat`, without copying.
   *
   *  @param n the exact number of elements that will be added to the builder
   */
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

  /** Returns a builder for `Double` elements.
   *
   *  The builder stores elements in an `Array[Double]` of length `n`;
   *  `result()` wraps that array in an `ArraySeq.ofDouble`, without copying.
   *
   *  @param n the exact number of elements that will be added to the builder
   */
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

  /** Returns a builder for `Boolean` elements.
   *
   *  The builder stores elements in an `Array[Boolean]` of length `n`;
   *  `result()` wraps that array in an `ArraySeq.ofBoolean`, without copying.
   *
   *  @param n the exact number of elements that will be added to the builder
   */
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

  /** Returns a builder for `Unit` elements.
   *
   *  The builder stores elements in an `Array[Unit]` of length `n`; `result()`
   *  wraps that array in an `ArraySeq.ofUnit`, without copying.
   *
   *  @param n the exact number of elements that will be added to the builder
   */
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