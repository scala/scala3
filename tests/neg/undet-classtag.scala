import scala.reflect.ClassTag

object Test {
  def f[T: reflect.ClassTag](x: T) = ???

  f(???) // error: undetermined ClassTag
}

// SI 9754
object Program {
  def test[T: ClassTag](x: T) = {
    val arr = new Array[T](1)
    println(arr.getClass)
    println(x.getClass)
    arr(0) = x
  }

  def main(args: Array[String]): Unit = {
    test(new Array[Nothing](0)) // error: undetermined ClassTag
  }
}

// SI 5353
object t5353 {
  if (false) Array("qwe") else Array() // error: undetermined ClassTag
}

