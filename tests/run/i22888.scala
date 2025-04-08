trait Foo:
  type A[T]
  var arr: Array[A[Int]] = null

class Bar() extends Foo:
  type A[T] = Int

trait Foo2:
  type Dummy
  type A[T] <: Dummy
  var arr: Array[A[Int]] = null

class Bar2() extends Foo2:
  type Dummy = Any
  type A[T] = Int

trait Foo3:
  type A[T] <: Object
  var arr: Array[A[String]] = null

class Bar3() extends Foo3:
  type A[T] = String

object Test:
  def main(args: Array[String]) =
    val bar = new Bar()
    bar.arr = Array.ofDim[Int](1)
    bar.arr(0) = 123

    val bar2 = new Bar2()
    bar2.arr = Array.ofDim[Int](1)
    bar2.arr(0) = 123

    val bar3 = new Bar3()
    bar3.arr = Array.ofDim[String](1)
    bar3.arr(0) = "123"

