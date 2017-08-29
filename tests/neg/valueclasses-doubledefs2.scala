class Meter(val x: Double) extends AnyVal

trait A {
  def apply(x: Double) = x.toString
}
trait B {
  def apply(x: Meter) = x.toString
}

object Test extends A with B // error: double definition
