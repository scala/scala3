case class Y[T](val y: T) extends AnyVal
case class Z()

object Test {
  def main(args: Array[String]) = {
    val a1 = Array(Array[Y[Z]]())
    val a2 = Array.ofDim[Y[Z]](2)
    val a3 = new Array[Y[Z]](5)
    val a4: Array[Y[Z]] = Array(Y[Z](Z()), Y[Z](Z()), Y[Z](Z()))

    println(a1.toList)
    println(a2.toList)
    println(a3.toList)
    println(a4.toArray.toList)
    println(a4.par.toList)
    println(a4.map(x => a4(0)).toList)
  }
}