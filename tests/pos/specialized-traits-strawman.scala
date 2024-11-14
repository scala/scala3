import language.experimental.erasedDefinitions

// Source code

/*inline*/ trait Iterator[T/*: Specialized*/]:
  def hasNext: Boolean
  def next(): T

/*inline*/ trait ArrayIterator[T/*: Specialized*/](elems: Array[T]) extends Iterator[T]:
  private var current = 0
  def hasNext: Boolean = current < elems.length
  def next(): T = try elems(current) finally current += 1

/*inline*/ trait Iterable[T/*: Specialized*/]:
  def iterator: Iterator[T]
  def forall(f: T => Unit): Unit =
    val it = iterator
    while it.hasNext do f(it.next())

/*inline*/ trait Vec[T/*: Specialized*/](elems: Array[T])
extends Iterable[T]:
  def length: Int           = elems.length
  def apply(i: Int): T      = elems(i)
  def iterator: Iterator[T] = new ArrayIterator[T](elems) {}

// Specialized trait APIs generated from specialization:

trait Iterator_Int extends Iterator[Int]:
  def hasNext: Boolean
  def next(): Int

trait ArrayIterator_Int extends ArrayIterator[Int], Iterator[Int]

trait Iterable_Int extends Iterable[Int]:
  def iterator: Iterator_Int
  def forall(f: Int => Unit): Unit

trait Vec_Int extends Vec[Int], Iterable[Int]:
  def length: Int
  def apply(i: Int): Int

class ArrayIterator_Int$impl(elems: Array[Int]) extends ArrayIterator_Int
    , ArrayIterator[Int](elems): // snd parent not needed in actual translation
  private var current = 0
  override def hasNext: Boolean =
    current < elems.length
  override def next(): Int =
    try elems(current) finally current += 1

class Vec_Int$impl(elems: Array[Int]) extends Vec_Int
    , Vec[Int](elems):        // snd parent not needed in actual translation
  override def iterator: Iterator_Int =
    new ArrayIterator_Int$impl(elems).asInstanceOf
      // cast needed since the compiler does not not know that Iterable[Int] = Iterable_Int
      // after erasure. No cast would be needed in the actual translation.

  override def forall(f: Int => Unit): Unit =
    val it = iterator
    while it.hasNext do f(it.next())
  override def length: Int = elems.length
  override def apply(i: Int): Int = elems(i)

// Inline trait APIs generated from inline traits transform:
object InlineTraitAPIs:

  trait Iterator[T]:
    def hasNext: Boolean
    def next(): T

  trait ArrayIterator[T] extends Iterator[T]

  trait Iterable[T]:
    def iterator: Iterator[T]
    def forall(f: T => Unit): Unit

  trait Vec[T] extends Iterable[T]:
    def length: Int
    def apply(i: Int): T
end InlineTraitAPIs

/* Code after erasure: */
object AfterErasure:

  trait Function1:
    def apply(x: Any): Any
    def apply_Int_Unit(x: Int): Unit

  trait Iterator:
    def hasNext: Boolean
    def next(): Any

  trait ArrayIterator extends Iterator

  trait Iterable:
    def iterator: Iterator
    def forall(f: Function1): Unit

  trait Vec:
    def length: Int
    def apply(i: Int): Any

  trait Iterator_Int extends Iterator:
    def hasNext: Boolean
    def next(): Int

  trait ArrayIterator_Int extends ArrayIterator, Iterator_Int

  trait Iterable_Int extends Iterable:
    def iterator: Iterator_Int
    def forall(f: Function1): Unit

  trait Vec_Int extends Vec, Iterable_Int:
    def length: Int
    def apply(i: Int): Int

  class ArrayIterator_Int$impl(elems: Array[Int]) extends ArrayIterator_Int:
    private var current = 0
    override def hasNext: Boolean =
      current < elems.length
    override def next(): Int =
      try elems(current) finally current += 1

    /* Bridges:
    override def next(): Any = Int.box(next())
    */
  end ArrayIterator_Int$impl

  class Vec_Int$impl(elems: Array[Int]) extends Vec_Int:
    override def iterator: Iterator_Int =
      new ArrayIterator_Int$impl(elems)
    override def forall(f: Function1): Unit =
      val it = iterator
      while it.hasNext do f.apply_Int_Unit(it.next())
    override def length: Int = elems.length
    override def apply(i: Int): Int = elems(i)

    /* Bridges:
    override def iterator: Iterator = iterator
    override def apply(i: Int): Any = Int.box(apply(i))
    */
  end Vec_Int$impl

