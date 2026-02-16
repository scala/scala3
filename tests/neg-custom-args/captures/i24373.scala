trait Foo:
  consume def sink1: Unit = ()

trait Bar extends Foo, caps.Stateful:
  consume def sink2: Unit = ()

class C extends caps.Stateful:
  def sink4(consume x: Foo^) = ()

object Foo:
  extension (consume self: Foo^)
    def sink: Unit = ()

def Test =

  def sink3(consume self: Foo^): Unit = ()

  def sink5(consume self: Bar): Unit = ()

  def sink6(consume self: Bar^): Unit = ()

  val x: Bar^ = new Bar {}
  x.sink
  x.sink // no error: rd/rd

  val x1: Foo^ = new Foo {}
  x1.sink1
  x1.sink1 // error

  val x2: Bar^ = new Bar {}
  x2.sink2
  x2.sink2 // error

  val x3: Bar^ = new Bar {}
  sink3(x3)
  sink3(x3) // no error: rd/rd

  val x4: Bar^ = new Bar {}
  val c = C()
  c.sink4(x4)
  c.sink4(x4) // no error: rd/rd

  val x5: Bar^ = new Bar {}
  sink5(x5)
  sink5(x5) // no error: rd/rd

  val x6: Bar^ = new Bar {}
  sink6(x6)
  sink6(x6) // error



