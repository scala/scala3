import scala.quoted.*
import Macros.*

object Test {
  def main(args: Array[String]): Unit = {

    val sym = new Symantics2

    val test = m(sym)
  }
}

class Symantics2 extends Symantics {
  def Meth(exp: Int): Int = exp
  def Meth(): Int = 42
}
