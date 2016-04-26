case class Y(val y: Object) extends AnyVal
case class Z()
object Test {
  def main(args: Array[String]) = {
    def a1 = Array(Array[Y]())
    def a2 = Array.ofDim[Y](2)
    def a3 = new Array[Y](5)
    def a4: Array[Y] = Array(Y(Z()), Y(Z()), Y(Z()))
    println(a4.toArray.toList)
    println(a4.:+(Y(Z())).toList)
    println(a4.+:(Y(Z())).toList)
    println(a4.par.toList)
    val a5 = Array(Y(Z()), Y(Z()))
    val a6 = Array(Y(Z()), Y(Z()))
    println(a5.map(x => a6(0)).toList)
  }
}