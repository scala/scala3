import scala.quoted._
import Macros._
import scala.quoted.given

object Test {
  def main(args: Array[String]) = {
    var x = 1
    println(assignmentPatMat({x = 3}))
    ()
  }
}