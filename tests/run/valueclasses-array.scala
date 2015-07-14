class Meter(val underlying: Int) extends AnyVal {
  def plus(other: Meter): Meter =
    new Meter(underlying + other.underlying)

  override def toString = "Meter(" + underlying + ")"
}

object Test {
  def genAccess[T](genArray: Array[T]) = genArray(0)
  def genUpdated[T](genArray: Array[T], elem: T) = genArray(0) = elem
  def genLength[T](genArray: Array[T]) = genArray.length

  def foo(myArray: Array[Meter], elem: Meter) = {
    val update = myArray(0) = elem
    val access = myArray(0)
    val length: Int = myArray.length

    println(access)
    println(length)
  }

 def genFoo[T](genArray: Array[T], elem: T) = {
    val update = genArray(0) = elem
    val access = genArray(0)
    val length: Int = genArray.length

    println(access)
    println(length)
  }

  def main(args: Array[String]) = {
    val myArray = new Array[Meter](1)
    val myArray2 = Array[Meter](new Meter(1), new Meter(2))
    assert(myArray.getClass eq myArray2.getClass)

    println(myArray.toString)
    foo(myArray, new Meter(1))
    foo(myArray2, new Meter(2))
    genFoo(myArray, new Meter(3))
    genFoo(myArray2, new Meter(4))
  }
}
