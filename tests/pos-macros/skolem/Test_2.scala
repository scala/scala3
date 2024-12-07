trait Foo:
  val x: Int
  def ho(p: x.type => x.type): Unit = ()

object Test {
  var f: Foo = ???
  Macro.macr:
    f.ho(arg => arg)
}
