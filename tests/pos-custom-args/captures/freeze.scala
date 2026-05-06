import language.experimental.captureChecking
import caps.*

class Arr[T: reflect.ClassTag](len: Int) extends Mutable:
  private val arr: Array[T]^ = new Array[T](len)
  def get(i: Int): T = arr(i)
  update def update(i: Int, x: T): Unit = arr(i) = x


def test2 =
  val a = freeze:
    val a = Arr[String](2)
    a(0) = "1"
    a(1) = "2"
    a
  val _: Arr[String]^{} = a

def test3 =
  val a = Arr[String](2)
  a(0) = "1"
  a(1) = "2"
  val b = freeze(a)
  val _: Arr[String]^{} = b
  b

def test4 =
  val a = freeze:
    val a = new Array[String](2)
    a(0) = "1"
    a(1) = "2"
    a
  val _: Array[String]^{} = a

def test5 =
  val a = new Array[String](2)
  a(0) = "1"
  a(1) = "2"
  val b = freeze(a)
  val _: Array[String]^{} = b
  b

