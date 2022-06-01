import scala.annotation.newMain

class MyNumber(val value: Int) {
  def +(other: MyNumber): MyNumber = MyNumber(value + other.value)
}

object myProgram:

  @newMain def add(num: MyNumber, inc: MyNumber): Unit = // error
    println(s"$num + $inc = ${num + inc}")

end myProgram
