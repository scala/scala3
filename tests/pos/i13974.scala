object Test {
  class C
  class Use[A]
  case class UseC() extends Use[C]
  class ConversionTarget
  implicit def convert(c: C): ConversionTarget = ???
  def go[X](u: Use[X], x: X) =
    u match {
      case UseC() =>
        //val y: C = x
        x: ConversionTarget
    }
}
