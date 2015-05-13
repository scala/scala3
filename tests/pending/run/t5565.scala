import scala.language.reflectiveCalls
import scala.language.implicitConversions

object Test extends App {
  implicit def doubleWithApproxEquals(d: Double): AnyRef{def ~==(v: Double,margin: Double): Boolean; def ~==$default$2: Double @scala.annotation.unchecked.uncheckedVariance} = new {
    def ~==(v: Double, margin: Double = 0.001): Boolean =
      math.abs(d - v) < margin
  }

  assert(math.abs(-4.0) ~== (4.0, 0.001))
  assert(math.abs(-4.0) ~== 4.0)
}
