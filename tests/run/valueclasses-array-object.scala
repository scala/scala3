class MeterObj(val underlying: Object) extends AnyVal {
  def plus(other: MeterObj): MeterObj =
    new MeterObj("plus-object: " + underlying.toString + other.underlying.toString)

  override def toString = "Meter(" + underlying + ")"
}
class MeterStr(val value: String) extends AnyVal {
  def plus(other: MeterStr): MeterStr =
    new MeterStr("plus-object: " + value + other.value)

  override def toString = "Meter(" + value + ")"
}

object Test {
  def genAccess[T](genArray: Array[T]) = genArray(0)
  def genUpdated[T](genArray: Array[T], elem: T) = genArray(0) = elem
  def genLength[T](genArray: Array[T]) = genArray.length

  def fooObj(myArray: Array[MeterObj], elem: MeterObj) = {
    val update = myArray(0) = elem
    val access = myArray(0)
    val length: Int = myArray.length

    println(access)
    println(length)
  }

  def fooStr(myArray: Array[MeterStr], elem: MeterStr) = {
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
    val objArray = new Array[MeterObj](1)
    val objArray2 = Array[MeterObj](new MeterObj("obj1"), new MeterObj("obj2"))
    assert(objArray.getClass eq objArray2.getClass)

    println(objArray.toString)
    fooObj(objArray, new MeterObj("obj1"))
    fooObj(objArray2, new MeterObj("obj2"))
    genFoo(objArray, new MeterObj("obj3"))
    genFoo(objArray2, new MeterObj("obj4"))

    val strArray = new Array[MeterStr](1)
    val strArray2 = Array[MeterStr](new MeterStr("str1"), new MeterStr("str2"))
    assert(strArray.getClass eq strArray2.getClass)

    println(strArray.toString)
    fooStr(strArray, new MeterStr("str1"))
    fooStr(strArray2, new MeterStr("str2"))
    genFoo(strArray, new MeterStr("str3"))
    genFoo(strArray2, new MeterStr("str4"))
  }
}