class Foo {
  def b = {
    name.size                   // error
    lazy val m = name.size      // error: triggered from forcing `m`
    def bar = name.size         // error: triggered from calling `bar`
    bar                         // error: trigger non-init
    m                           // error: trigger
  }

  b    // error

  val name = "Jack"
}