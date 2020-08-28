trait Bundle extends reflect.Selectable

val foo = new Bundle {
  object Foo {
    def bar = 23
  }
}

val bar = new Bundle {
  class Xyz extends Bundle { def xyz = 31 }
  val ref: Xyx = Xyz() // error: Not found: type Xyx
}

val baz = new Bundle {
  class Xyz { def xyz = 31 }
  val ref: Xyz = Xyz()
}

def test =
  assert(foo.Foo.bar == 23) // error: bar is not a member of Object
  assert(baz.ref.xyz) // error: xyz is not a member of Object
