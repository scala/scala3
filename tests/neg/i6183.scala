object Test:
  extension [A](a: A) def render: String = "Hi"
  extension [B](b: B) def render(using DummyImplicit): Char = 'x'

  val test = {
    42.render // error
    Test.extension_render(42) // error
  }
