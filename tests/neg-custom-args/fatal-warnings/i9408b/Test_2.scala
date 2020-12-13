import language.`3.0-migration`
import scala.language.implicitConversions

object Test {
  import test.conversions.Conv._
  val length: Int = "abc" // error
}
