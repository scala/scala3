import language.experimental.erasedDefinitions

erased class CanThrow[-E <: Exception]
erased class Foo
class Bar

object unsafeExceptions:
  given canThrowAny: CanThrow[Exception] = null

object test1:
  trait Decoder[+T]:
    def apply(): T

  def deco: Decoder[CanThrow[Exception] ?=> Int] = new Decoder[CanThrow[Exception] ?=> Int]:
    def apply(): CanThrow[Exception] ?=> Int = 1

object test2:
  trait Decoder[+T]:
    def apply(): T

  def deco: Decoder[(CanThrow[Exception], Foo) ?=> Int] = new Decoder[(CanThrow[Exception], Foo) ?=> Int]:
    def apply(): (CanThrow[Exception], Foo) ?=> Int = 1

object test3:
  trait Decoder[+T]:
    def apply(): T

  def deco: Decoder[CanThrow[Exception] ?=> Foo ?=> Int] = new Decoder[CanThrow[Exception] ?=> Foo ?=> Int]:
    def apply(): CanThrow[Exception] ?=> Foo ?=> Int = 1

object test4:
  trait Decoder[+T]:
    def apply(): T

  def deco: Decoder[CanThrow[Exception] ?=> Bar ?=> Int] = new Decoder[CanThrow[Exception] ?=> Bar ?=> Int]:
    def apply(): CanThrow[Exception] ?=> Bar ?=> Int = 1

object test5:
  trait Decoder[+T]:
    def apply(): T

  def deco: Decoder[Bar ?=> CanThrow[Exception] ?=> Int] = new Decoder[Bar ?=> CanThrow[Exception] ?=> Int]:
    def apply(): Bar ?=> CanThrow[Exception] ?=> Int = 1

@main def Test(): Unit =
  import unsafeExceptions.canThrowAny
  given Foo = ???
  given Bar = Bar()
  test1.deco.apply().apply
  test2.deco.apply().apply
  test3.deco.apply().apply
  test4.deco.apply().apply
  test5.deco.apply().apply
