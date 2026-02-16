import caps.*
object Test

class Arr[T: reflect.ClassTag](a: Async, len: Int) extends Stateful:
  private val arr: Array[T]^ = new Array[T](len)
  def get(i: Int): T = arr(i)
  update def set(i: Int, x: T): Unit = arr(i) = x

class Async extends SharedCapability

def f[T](x: T): T & Pure = x.asInstanceOf[T & Pure]

def test =
  def x(async: Async): Arr[String]^{any.rd} =
    val y = Arr[String](async, 10)
    for i <- 0 to 10 do
      y.set(i, "A")
    val z = f(y)
    y



