class Meter1[T](x: T) extends AnyVal {
  override def toString = "Meter1(" + x + ")"
}
class Meter2[T >: Null <: String](x: T) extends AnyVal {
  override def toString = "Meter2(" + x + ")"
}
class Meter3[T](x: Int) extends AnyVal {
  override def toString = "Meter3(" + x + ")"
}
class Meter4[C[T], Y](x: C[Y]) extends AnyVal {
  override def toString = "Meter4(" + x + ")"
}

object Test {
  def genAccess[T](genArray: Array[T]) = genArray(0)
  def genUpdated[T](genArray: Array[T], elem: T) = genArray(0) = elem
  def genLength[T](genArray: Array[T]) = genArray.length

  def foo1(myArray: Array[Meter1], elem: Meter1) = {
    val update = myArray(0) = elem
    val access = myArray(0)
    val length: Int = myArray.length

    println(access)
    println(length)
  }

  def foo2(myArray: Array[Meter2], elem: Meter2) = {
    val update = myArray(0) = elem
    val access = myArray(0)
    val length: Int = myArray.length

    println(access)
    println(length)
  }

  def foo3(myArray: Array[Meter3], elem: Meter3) = {
    val update = myArray(0) = elem
    val access = myArray(0)
    val length: Int = myArray.length

    println(access)
    println(length)
  }

  def foo4(myArray: Array[Meter4], elem: Meter4) = {
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
    val meter1Array = new Array[Meter1](1)
    val meter1Array2 = Array[Meter1](new Meter1(1), new Meter1(2))
    assert(meter1Array.getClass eq meter1Array2.getClass)

    println(meter1Array.toString)
    foo1(meter1Array, new Meter1(1))
    foo1(meter1Array2, new Meter1(2))
    genFoo(meter1Array, new Meter1(3))
    genFoo(meter1Array2, new Meter1(4))

    val meter2Array = new Array[Meter2](1)
    val meter2Array2 = Array[Meter2](new Meter2("m2-1"), new Meter2("m2-2"))
    assert(meter2Array.getClass eq meter2Array2.getClass)

    println(meter2Array.toString)
    foo2(meter2Array, new Meter2("m2-1"))
    foo2(meter2Array2, new Meter2("m2-2"))
    genFoo(meter2Array, new Meter2("m2-3"))
    genFoo(meter2Array2, new Meter2("m2-4"))

    val meter3Array = new Array[Meter3](1)
    val meter3Array2 = Array[Meter3](new Meter3(1), new Meter3(2))
    assert(meter3Array.getClass eq meter3Array2.getClass)

    println(meter3Array.toString)
    foo3(meter3Array, new Meter3(1))
    foo3(meter3Array2, new Meter3(2))
    genFoo(meter3Array, new Meter3(3))
    genFoo(meter3Array2, new Meter3(4))

    val meter4Array = new Array[Meter4](1)
    val meter4Array2 = Array[Meter4](new Meter4(List(1)), new Meter4(List(2)))
    assert(meter4Array.getClass eq meter4Array2.getClass)

    println(meter4Array.toString)
    foo4(meter4Array, new Meter4(List(1)))
    foo4(meter4Array2, new Meter4(List(2)))
    genFoo(meter4Array, new Meter4(List(3)))
    genFoo(meter4Array2, new Meter4(List(4)))
  }
}
