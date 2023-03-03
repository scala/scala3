trait Bar { type Y }
trait Foo { type X }

class Test:
  given a1(using b: Bar): Foo = new Foo { type X = b.Y }

  given a2(using b: Bar): Foo { type X = b.Y } = new Foo { type X = b.Y }

  given a3(using b: Bar): (Foo { type X = b.Y }) = new Foo { type X = b.Y }
