

object Test extends dotty.runtime.LegacyApp {
  JavaTest.main(null)

  var a1 : SomeClass = new SomeClass
  var b1 : Object =  a1.f.set(23)
}
