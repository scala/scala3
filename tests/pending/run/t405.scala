object Test extends dotty.runtime.LegacyApp {
  val x = M;
  object M;
  assert(x eq M)
}
