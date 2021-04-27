import scala.quoted.*

object Macro {

  inline def f64Pow5Split: Array[Long] = ${ f64Pow5SplitExpr }
  private def f64Pow5SplitExpr(using Quotes): Expr[Array[Long]] = Expr {
    val ss = new Array[Long](652)
    var pow5 = BigInt(1)
    var i = 0
    while (i < 652) {
      ss(i) = (pow5 >> (pow5.bitLength - 121)).longValue & 0x3FFFFFFFFFFFFFFFL
      ss(i + 1) = (pow5 >> (pow5.bitLength - 59)).longValue & 0x3FFFFFFFFFFFFFFFL
      pow5 *= 5
      i += 2
    }
    ss
  }

}
