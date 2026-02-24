import caps.*

class Arr[T: reflect.ClassTag](len: Int) extends Mutable:
  private val arr: Array[T]^ = new Array[T](len)
  def get(i: Int): T = arr(i)
  update def update(i: Int, x: T): Unit = arr(i) = x


def test2 =
  val a = Arr[String](2)
  val b = freeze((a, a)) // error

