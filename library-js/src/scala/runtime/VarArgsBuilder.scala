package scala.runtime

import scala.collection.immutable.ArraySeq
import scala.reflect.ClassTag

import scala.scalajs.js
import scala.scalajs.runtime.toScalaVarArgs
import scala.scalajs.LinkingInfo.{isWebAssembly, linkTimeIf}

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

  @inline
  private final class JSVarArgsBuilder[T] extends VarArgsBuilder[T]:
    private val array: js.Array[T] = js.Array()
    def add(elem: T): this.type =
      array.push(elem)
      this
    def addSeq(elems: Seq[T]): this.type =
      for elem <- elems do
        add(elem)
      this
    def addArray(elems: Array[T]): this.type =
      for elem <- elems do
        add(elem)
      this
    def result(): Seq[T] =
      toScalaVarArgs(array)

  /** Returns a builder for elements of a type not statically known to be a
   *  primitive or a reference type, such as an unbounded type parameter.
   *
   *  The implementation is chosen at link time: when targeting WebAssembly, a
   *  builder backed by a fixed-size `Array[AnyRef]` of length `n`; when
   *  targeting JavaScript, a builder backed by a growable `js.Array`, in which
   *  case `n` is unused.
   *
   *  @tparam T the element type
   *  @param n the exact number of elements that will be added to the builder;
   *           used only when targeting WebAssembly
   */
  @inline
  def generic[T](n: Int): VarArgsBuilder[T] =
    linkTimeIf[VarArgsBuilder[T]](isWebAssembly)(GenericVarArgsBuilder(n))(JSVarArgsBuilder())

  @inline
  private final class GenericVarArgsBuilder[T](n: Int) extends VarArgsBuilder[T]:
    private val xs = new Array[AnyRef](n)
    def result(): Seq[T] = ArraySeq.ofRef(xs).asInstanceOf[ArraySeq[T]]
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
   *  The implementation is chosen at link time: when targeting WebAssembly, a
   *  builder backed by a fixed-size `Array[AnyRef]` of length `n`; when
   *  targeting JavaScript, a builder backed by a growable `js.Array`, in which
   *  case `n` is unused.
   *
   *  @tparam T the element type, a reference type
   *  @param n the exact number of elements that will be added to the builder;
   *           used only when targeting WebAssembly
   */
  def ofRef[T <: AnyRef](n: Int): VarArgsBuilder[T] =
    linkTimeIf[VarArgsBuilder[T]](isWebAssembly)(RefVarArgsBuilder(n))(JSVarArgsBuilder())

  @inline
  private final class RefVarArgsBuilder[T <: AnyRef](n: Int) extends VarArgsBuilder[T]:
    private val xs = new Array[AnyRef](n)
    def result(): Seq[T] = ArraySeq.ofRef(xs).asInstanceOf[ArraySeq[T]]
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
   *  The implementation is chosen at link time: when targeting WebAssembly, a
   *  builder backed by a fixed-size `Array[Byte]` of length `n`; when
   *  targeting JavaScript, a builder backed by a growable `js.Array`, in which
   *  case `n` is unused.
   *
   *  @param n the exact number of elements that will be added to the builder;
   *           used only when targeting WebAssembly
   */
  def ofByte(n: Int): VarArgsBuilder[Byte] =
    linkTimeIf[VarArgsBuilder[Byte]](isWebAssembly)(ByteVarArgsBuilder(n))(JSVarArgsBuilder())

  @inline
  private final class ByteVarArgsBuilder(n: Int) extends VarArgsBuilder[Byte]:
    private val xs = new Array[Byte](n)
    def result(): Seq[Byte] = ArraySeq.ofByte(xs)
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
   *  The implementation is chosen at link time: when targeting WebAssembly, a
   *  builder backed by a fixed-size `Array[Short]` of length `n`; when
   *  targeting JavaScript, a builder backed by a growable `js.Array`, in which
   *  case `n` is unused.
   *
   *  @param n the exact number of elements that will be added to the builder;
   *           used only when targeting WebAssembly
   */
  def ofShort(n: Int): VarArgsBuilder[Short] =
    linkTimeIf[VarArgsBuilder[Short]](isWebAssembly)(ShortVarArgsBuilder(n))(JSVarArgsBuilder())

  @inline
  private final class ShortVarArgsBuilder(n: Int) extends VarArgsBuilder[Short]:
    private val xs = new Array[Short](n)
    def result(): Seq[Short] = ArraySeq.ofShort(xs)
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
   *  The implementation is chosen at link time: when targeting WebAssembly, a
   *  builder backed by a fixed-size `Array[Char]` of length `n`; when
   *  targeting JavaScript, a builder backed by a growable `js.Array`, in which
   *  case `n` is unused.
   *
   *  @param n the exact number of elements that will be added to the builder;
   *           used only when targeting WebAssembly
   */
  def ofChar(n: Int): VarArgsBuilder[Char] =
    linkTimeIf[VarArgsBuilder[Char]](isWebAssembly)(CharVarArgsBuilder(n))(JSVarArgsBuilder())

  @inline
  private final class CharVarArgsBuilder(n: Int) extends VarArgsBuilder[Char]:
    private val xs = new Array[Char](n)
    def result(): Seq[Char] = ArraySeq.ofChar(xs)
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
   *  The implementation is chosen at link time: when targeting WebAssembly, a
   *  builder backed by a fixed-size `Array[Int]` of length `n`; when targeting
   *  JavaScript, a builder backed by a growable `js.Array`, in which case `n`
   *  is unused.
   *
   *  @param n the exact number of elements that will be added to the builder;
   *           used only when targeting WebAssembly
   */
  def ofInt(n: Int): VarArgsBuilder[Int] =
    linkTimeIf[VarArgsBuilder[Int]](isWebAssembly)(IntVarArgsBuilder(n))(JSVarArgsBuilder())

  @inline
  private final class IntVarArgsBuilder(n: Int) extends VarArgsBuilder[Int]:
    private val xs = new Array[Int](n)
    def result(): Seq[Int] = ArraySeq.ofInt(xs)
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
   *  The implementation is chosen at link time: when targeting WebAssembly, a
   *  builder backed by a fixed-size `Array[Long]` of length `n`; when
   *  targeting JavaScript, a builder backed by a growable `js.Array`, in which
   *  case `n` is unused.
   *
   *  @param n the exact number of elements that will be added to the builder;
   *           used only when targeting WebAssembly
   */
  def ofLong(n: Int): VarArgsBuilder[Long] =
    linkTimeIf[VarArgsBuilder[Long]](isWebAssembly)(LongVarArgsBuilder(n))(JSVarArgsBuilder())

  @inline
  private final class LongVarArgsBuilder(n: Int) extends VarArgsBuilder[Long]:
    private val xs = new Array[Long](n)
    def result(): Seq[Long] = ArraySeq.ofLong(xs)
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
   *  The implementation is chosen at link time: when targeting WebAssembly, a
   *  builder backed by a fixed-size `Array[Float]` of length `n`; when
   *  targeting JavaScript, a builder backed by a growable `js.Array`, in which
   *  case `n` is unused.
   *
   *  @param n the exact number of elements that will be added to the builder;
   *           used only when targeting WebAssembly
   */
  def ofFloat(n: Int): VarArgsBuilder[Float] =
    linkTimeIf[VarArgsBuilder[Float]](isWebAssembly)(FloatVarArgsBuilder(n))(JSVarArgsBuilder())

  @inline
  private final class FloatVarArgsBuilder(n: Int) extends VarArgsBuilder[Float]:
    private val xs = new Array[Float](n)
    def result(): Seq[Float] = ArraySeq.ofFloat(xs)
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
   *  The implementation is chosen at link time: when targeting WebAssembly, a
   *  builder backed by a fixed-size `Array[Double]` of length `n`; when
   *  targeting JavaScript, a builder backed by a growable `js.Array`, in which
   *  case `n` is unused.
   *
   *  @param n the exact number of elements that will be added to the builder;
   *           used only when targeting WebAssembly
   */
  def ofDouble(n: Int): VarArgsBuilder[Double] =
    linkTimeIf[VarArgsBuilder[Double]](isWebAssembly)(DoubleVarArgsBuilder(n))(JSVarArgsBuilder())

  @inline
  private final class DoubleVarArgsBuilder(n: Int) extends VarArgsBuilder[Double]:
    private val xs = new Array[Double](n)
    def result(): Seq[Double] = ArraySeq.ofDouble(xs)
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
   *  The implementation is chosen at link time: when targeting WebAssembly, a
   *  builder backed by a fixed-size `Array[Boolean]` of length `n`; when
   *  targeting JavaScript, a builder backed by a growable `js.Array`, in which
   *  case `n` is unused.
   *
   *  @param n the exact number of elements that will be added to the builder;
   *           used only when targeting WebAssembly
   */
  def ofBoolean(n: Int): VarArgsBuilder[Boolean] =
    linkTimeIf[VarArgsBuilder[Boolean]](isWebAssembly)(BooleanVarArgsBuilder(n))(JSVarArgsBuilder())

  @inline
  private final class BooleanVarArgsBuilder(n: Int) extends VarArgsBuilder[Boolean]:
    private val xs = new Array[Boolean](n)
    def result(): Seq[Boolean] = ArraySeq.ofBoolean(xs)
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
   *  The implementation is chosen at link time: when targeting WebAssembly, a
   *  builder backed by a fixed-size `Array[Unit]` of length `n`; when
   *  targeting JavaScript, a builder backed by a growable `js.Array`, in which
   *  case `n` is unused.
   *
   *  @param n the exact number of elements that will be added to the builder;
   *           used only when targeting WebAssembly
   */
  def ofUnit(n: Int): VarArgsBuilder[Unit] =
    linkTimeIf[VarArgsBuilder[Unit]](isWebAssembly)(UnitVarArgsBuilder(n))(JSVarArgsBuilder())

  @inline
  private final class UnitVarArgsBuilder(n: Int) extends VarArgsBuilder[Unit]:
    private val xs = new Array[Unit](n)
    def result(): Seq[Unit] = ArraySeq.ofUnit(xs)
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
