
import scala.quoted._
import scala.quoted.staging._

object Test {
  given Toolbox = Toolbox.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = {
    for (n <- 0 to 22) { // FIXME should go up to 25. This should be fixed in #9984
      prev = 0
      println(run { Expr.ofTupleFromSeq(Seq.fill(n)('{next})) })
    }
  }
  var prev = 0
  def next: Int = {
    prev += 1
    prev
  }
}
