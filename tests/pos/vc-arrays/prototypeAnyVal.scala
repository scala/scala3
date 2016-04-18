case class Meter(x: Int) extends AnyVal
class Test {
  //prototypes and companions should be rewired to pass -Ycheck:all
  val x: AnyVal = Meter(1)
}