import language.experimental.captureChecking
import caps.*

class Arr[T: reflect.ClassTag](len: Int) extends Mutable:
  private val arr: Array[T] = new Array[T](len)
  def get(i: Int): T = arr(i)
  update def update(i: Int, x: T): Unit = arr(i) = x


def test2 =
  val a = immutable:
    val a = Arr[String](2)
    a(0) = "1"
    a(1) = "2"
    a
  val _: Arr[String]^{} = a

  val a2 = immutable:
    val a = Arr[String](2)
    val b = Arr[String](2)
    a(0) = "1"
    a(1) = "2"
    b(0) = "1"
    b(1) = "2"
    (a, b)
  val _: (Arr[String]^{}, Arr[String]^{}) = a2
