trait Foo extends reflect.Selectable
object Test:
  val foo = new Foo:
    object pin:
      val x = 1
  export foo.pin.* // error: (because we need reflection to get at foo.pin)

object OK:
  object Foo:
    object pin:
      val x = 1
  export Foo.pin.*
