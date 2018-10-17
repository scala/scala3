import scala.quoted._

object Test {
  def main(args: Array[String]): Unit = {
    val tb = Toolbox.make
    tb.run {
      val a = '{
        def z: Int = 5
        Macro.ff(z, 5)
      }
      a.show.toExpr
    }
  }
}
