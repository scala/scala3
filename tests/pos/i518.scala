class Meter(val underlying: Int) extends AnyVal

class Test {
  val x: Int = new Meter(3).hashCode()
   // After phase VCInline the rhs should be expanded to Meter.hashCode$extension(3)
}
