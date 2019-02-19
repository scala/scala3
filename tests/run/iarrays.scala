import reflect.ClassTag
object Test extends App {

  val xs = IArray(1, 2, 3)

  def f[T](ys: IArray[T]) = {
    assert(ys.length == 3)
    var sum = 0
    for (i <- 0 until ys.length)
      sum += xs(i)
    assert(sum == 6)
  }

  f(xs)

  var sum = 0
  for (i <- 0 until xs.length)
    sum += xs(i)
  assert(sum == 6)

  def reduce[T](xs: IArray[T], z: T, op: (T, T) => T) = {
    var acc = z
    for (i <- 0 until xs.length)
      acc = op(acc, xs(i))
    acc
  }

  def reduce2[T <: AnyRef](xs: IArray[T], z: T, op: (T, T) => T) = {
    var acc = z
    for (i <- 0 until xs.length)
      acc = op(acc, xs(i))
    acc
  }

  def flatten[T: ClassTag](ys: IArray[IArray[T]]) = {
    var len = 0
    for (i <- 0 until ys.length) len += ys(i).length
    val flat = new Array[T](len)
    var k = 0
    for (i <- 0 until ys.length) {
      for (j <- 0 until ys(i).length) {
        flat(k) = ys(i)(j)
        k += 1
      }
    }
    IArray(flat: _*)
  }

  val ys = IArray.concat(xs, xs, xs)
  assert(reduce(ys, 0, _ + _) == 18)

  val ss = IArray("a", "b", "c")
  assert(reduce2(ss, "", _ ++ _) == "abc")

  val zss = IArray.fill(2, 3)(1)
  val zs = flatten(zss)
  assert(reduce(zs, 0, _ + _) == 6)

  val is = IArray.iterate(0, 4)(_ + 1)
  assert(reduce(is, 0, _ + _) == 6)

  val IArray(1, 2, 3) = xs

  val as: IArray[Any] = IArray(1, "hello")
  assert(as(as.length - 1) == "hello")
  assert(reduce(as, 0, (x, y) => x.toString ++ y.toString) == "01hello")

  // Check that representation of IArray and Array is the same
  val bs: IArray[Double] = IArray(1.0, 2.0)
  val cs: Array[Double] = bs.asInstanceOf[Array[Double]]
  cs(1) = 3.0
  assert(bs(1) == 3.0)
}