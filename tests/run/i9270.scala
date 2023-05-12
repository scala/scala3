// https://github.com/lampepfl/dotty/issues/9270
class C:
  object Foo extends Foo.Bar.Base:
    object Bar:
      class Base

@main def Test() =
  val c = new C
  val x = new c.Foo.Bar.Base
