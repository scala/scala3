import scala.language.experimental.tracked
import scala.language.future

trait WithValue { type Value = Int }

case class Year(value: Int) extends WithValue {
  val x: Value = 2
}
