class Foo:
  type Value
  def test: Option[Value] =
    val scr = {
      val self: Foo.this.type = this
      None: Option[self.Value]
    }
    scr
