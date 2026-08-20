import caps.*
import scala.reflect.ClassTag

class Arr[T: ClassTag](n: Int) extends Mutable:
  private val elems = new Array[T](n)
  def get(i: Int): T = elems.apply(i)
  update def update(i: Int, x: T): Unit = elems(i) = x

class Arr2[T: ClassTag](n: Int) extends Mutable:
  private val elems: Array[T]^ = new Array[T](n)
  def get(i: Int): T = elems.apply(i)
  update def update(i: Int, x: T): Unit = elems(i) = x