
import scala.quoted._
import scala.quoted.staging._

object Test {
  implicit val toolbox: scala.quoted.staging.Toolbox = scala.quoted.staging.Toolbox.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = {
    for (n <- 0 to 25) {
      prev = 0
      println(run { Seq.fill(n)('{next}).toExprOfTuple })
    }
  }
  var prev = 0
  def next: Int = {
    prev += 1
    prev
  }
}
