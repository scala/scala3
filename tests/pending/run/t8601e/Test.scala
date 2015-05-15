class C {
  def foo: Unit = {StaticInit.fld}
}

object Test extends dotty.runtime.LegacyApp {
  try {
    new C().foo
    sys.error("StaticInit.<clinit> was not run!")
  } catch {
    case t: ExceptionInInitializerError =>
  }
}
