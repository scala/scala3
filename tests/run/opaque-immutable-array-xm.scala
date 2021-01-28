import scala.reflect.ClassTag
object Test extends App {

  import java.util.Arrays

  opaque type IArray[A1] = Array[A1]

  implicit object IArray {
    def initialize[A](body: => Array[A]): IArray[A] = body
    def apply[A: ClassTag](xs: A*): IArray[A] = initialize(Array(xs*))

    // These should be inline but that does not work currently. Try again
    // once inliner is moved to PickleQuotes
    extension [A](ia: IArray[A]) def length: Int = (ia: Array[A]).length
    extension [A](ia: IArray[A]) def apply (i: Int): A = (ia: Array[A])(i)

    // return a sorted copy of the array
    def sorted[A <: AnyRef : math.Ordering](ia: IArray[A]): IArray[A] = {
      val arr = Arrays.copyOf(ia, (ia: Array[A]).length)
      scala.util.Sorting.quickSort(arr)
      arr
    }

    // use a standard java method to search a sorted IArray.
    // (note that this doesn't mutate the array).
    def binarySearch(ia: IArray[Long], elem: Long): Int =
      Arrays.binarySearch(ia, elem)
  }

  // same as IArray.binarySearch but implemented by-hand.
  //
  // given a sorted IArray, returns index of `elem`,
  // or a negative value if not found.
  def binaryIndexOf(ia: IArray[Long], elem: Long): Int = {
    var lower: Int = 0
    var upper: Int = ia.length - 1
    while (lower <= upper) {
      val middle = (lower + upper) >>> 1
      val n = ia(middle)

      if (n == elem) return middle
      else if (n < elem) lower = middle + 1
      else upper = middle - 1
    }
    -lower - 1
  }

  val xs: IArray[Long] = IArray(1L, 2L, 3L)
  assert(binaryIndexOf(xs, 2L) == 1)
  assert(binaryIndexOf(xs, 4L) < 0)
}