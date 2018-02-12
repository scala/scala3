abstract sealed class ArgNumber
case object IsList extends ArgNumber
case object ArgNumber

object Test extends dotty.runtime.LegacyApp {
  println(IsList)
}
