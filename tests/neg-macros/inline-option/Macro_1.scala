
import scala.quoted._
import scala.quoted.autolift._

object Macro {
  def impl(opt: Option[Int]): Staged[Int] = opt match {
    case Some(i) => i
    case None => '{-1}
  }
}