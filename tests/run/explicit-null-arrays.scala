
object Test {
  def sort[T <: AnyRef : Ordering](a: Array[T]): Array[T] = {
    import java.util.Arrays
    import scala.NonNull.ArrayConversions._
    val a2: Array[T] = Arrays.copyOf(a, a.length).nn
    scala.util.Sorting.quickSort(a2)
    a2
  }

  def main(args: Array[String]): Unit = {
    val a: Array[Integer] = Array(3, 2, 4, 1)
    val a2 = sort(a)
    assert(a2(0) == 1)
    assert(a2(1) == 2)
    assert(a2(2) == 3)
    assert(a2(3) == 4)
  }
}

