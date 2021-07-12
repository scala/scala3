object Test {
  def arr0[T](x: Array[T]) = {
    assert(x(0) == 0)
    x.sameElements(x)
    x match {
      case x: Array[Int] =>
        x(0) = 1
        x.sameElements(x)
    }
    assert(x(0) == 1)
  }

  def arr1(x: Array[Int]) = {
    assert(x(0) == 1)
    x.sameElements(x)
    x match {
      case x: Array[_] =>
        x(0) = 2
        x.sameElements(x)
    }
    assert(x(0) == 2)
  }

  def arr2[T](x: T) = {
    x match {
      case x: Array[_] =>
        assert(x(0) == 2)
        x.sameElements(x)
    }
    x match {
      case x: Array[Int] =>
        assert(x(0) == 2)
        x(0) = 3
        x.sameElements(x)
    }
  }

  def arr3[T <: Int](x: Array[T]) = {
    x(0) == 2
    x.sameElements(x)
  }

  def arr4[T <: Int | Double](x: Array[T]) = {
    x(0) == 2
    x.sameElements(x)
  }

  def arr5[T <: Int | String](x: Array[T]) = {
    x(0) == 2
    x.sameElements(x)
  }

  def arr6[T <: Matchable](x: Array[T]) = {
    x(0) == 2
    x.sameElements(x)
  }

  def main(args: Array[String]): Unit = {
    val x: Array[Int] = Array(0)

    arr0(x)
    arr1(x)
    arr2(x)
    arr3(x)
    arr4(x)
    arr5(x)
    arr6(x)


    val str: Any = ""
    assert(!str.isInstanceOf[Array[?]])
    assert(!str.isInstanceOf[Array[Array[?]]])
    assert(!str.isInstanceOf[Array[? <: Array[?]]])
  }
}
