
import Macros._


object Test {

  def main(args: Array[String]): Unit = {
    println(lift(StringNum)(LitDSL(1)))
    println(lift(ComputeNum)(LitDSL(1)))
    println(lift(ASTNum)(LitDSL(1)))
    println()
    println(lift(StringNum)(LitDSL(1) + LitDSL(2)))
    println(lift(ComputeNum)(LitDSL(1) + LitDSL(2)))
    println(lift(ASTNum)(LitDSL(1) + LitDSL(2)))
    println()
    println(lift(StringNum)(LitDSL(1) * LitDSL(2)))
    println(lift(ComputeNum)(LitDSL(1) * LitDSL(2)))
    println(lift(ASTNum)(LitDSL(1) * LitDSL(2)))
    println()
    println(lift(StringNum)(LitDSL(1) + LitDSL(3) * LitDSL(4)))
    println(lift(ComputeNum)(LitDSL(1) + LitDSL(3) * LitDSL(4)))
    println(lift(ASTNum)(LitDSL(1) + LitDSL(3) * LitDSL(4)))
  }

}

object StringNum extends Symantics[String] {
  def value(x: Int): String = x.toString
  def plus(x: String, y: String): String = s"($x + $y)"
  def times(x: String, y: String): String = s"($x * $y)"
}

object ComputeNum extends Symantics[Int] {
  def value(x: Int): Int = x
  def plus(x: Int, y: Int): Int = x + y
  def times(x: Int, y: Int): Int = x * y
}

object ASTNum extends Symantics[ASTNum] {
  def value(x: Int): ASTNum = LitAST(x)
  def plus(x: ASTNum, y: ASTNum): ASTNum = PlusAST(x, y)
  def times(x: ASTNum, y: ASTNum): ASTNum = TimesAST(x, y)
}

trait ASTNum
case class LitAST(x: Int) extends ASTNum
case class PlusAST(x: ASTNum, y: ASTNum) extends ASTNum
case class TimesAST(x: ASTNum, y: ASTNum) extends ASTNum
