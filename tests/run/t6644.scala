class Testable(val c: String) extends AnyVal {
  def matching(cases: Boolean*) = cases contains true
}

object Test extends dotty.runtime.LegacyApp {
  assert(new Testable("").matching(true, false))
}

