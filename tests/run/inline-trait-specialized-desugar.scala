// User code does this: (with Specialized type class)
inline trait Iterator[T]:
  def hasNext: Boolean
  def next(): T

// User code does this: (with Specialized type class)
inline trait ArrayIterator[T](elems: Array[T]) extends Iterator[T]:
  private var current: Int = 0
  def hasNext: Boolean = current < elems.length
  def next(): T = try elems(current) finally current += 1


// Specialized traits generates these signatures:
inline trait Iterator_sp_Int extends Iterator[Int]
inline trait ArrayIterator_sp_Int extends ArrayIterator[Int], Iterator_sp_Int
class ArrayIterator_impl_Int(elems: Array[Int]) extends ArrayIterator_sp_Int, ArrayIterator[Int](elems)

// User code does this:
def foo(x: ArrayIterator[Int]): Int = x.next()

// Specialized traits converts this to
def foo(x: ArrayIterator_sp_Int): Int = x.next()

// User code does this:
/* class MyClassA
   class MyClassB extends MyClassA, ArrayIterator[Int](Array.from(Seq(1, 5))) */

// We convert this to:
class MyClassA
class MyClassB extends MyClassA, ArrayIterator_sp_Int, ArrayIterator[Int](Array.from(Seq(1, 5)))

@main def Test = 
    val xs: Array[Int] = Array(1, 2, 3)

    // User code does this:
    /* val ai = new ArrayIterator[Int](xs) {} */

    // We convert this to:
    val ai = ArrayIterator_impl_Int(xs)
    
    val mcb = MyClassB()
    assert(mcb.hasNext)
    assert(ai.next() == 1)
