class Outer {
  class Foo(var unpack: Outer.this.type)

  type Smuggler
  var smuggler: Option[Smuggler] = None
}
class Evil(val outer: Outer, extract: outer.type => Unit) extends outer.Foo(outer) { // error
  def doExtract(): Unit = extract(unpack)
}
