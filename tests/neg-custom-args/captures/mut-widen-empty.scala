import caps.*

class Arr[T: reflect.ClassTag](len: Int) extends Mutable:
  private val arr: Array[T]^ = new Array[T](len)
  def get(i: Int): T = arr(i)
  update def update(i: Int, x: T): Unit = arr(i) = x


def test2 =
  val a = Arr[String](2)
  val b: Arr[String]^{} = ???
  val c: Arr[String]^ = b  // error
  c(2) = "a"   // boom!

def test3 =
  val a = new Array[String](2)
  val b: Array[String]^{} = ???
  val c: Array[String]^ = b  // error
  c(2) = "a"   // boom!

