//> using options -experimental

trait Mod

given Conversion[String, Mod] = _ => new Mod {}

type Foo = (name: String, mod: Mod)
case class Foo0(name: String, mod: Mod)

@main def Test =
  val foo: Foo = (name = "foo", mod = "some_mod")
  val foo_updated: Foo = foo.copyFrom((mod = "bar")) // error, stays as String


  val foo0: Foo0 = Foo0(name = "foo", mod = "some_mod")
  val foo0_updated: Foo0 = foo0.copy(mod = "bar") // ok - does the conversion
