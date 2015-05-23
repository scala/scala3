object Test extends dotty.runtime.LegacyApp {

  implicit class C(s: String) {
    def nElems = s.length
  }

  assert("abc".nElems == 3)

}

