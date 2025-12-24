trait Foo
trait Bar extends Foo, caps.Stateful

def Test =

  def sink1(consume self: Foo^): Unit = () // consumes read-only reference

  def sink2(consume self: Bar^): Unit = () // consumes exclusive reference

  def sink3(consume self: Bar): Unit = () // consumes read-only reference

  val x1: Bar^ = new Bar {}
  sink1(x1)
  sink1(x1)  // ok, rd/rd
  sink2(x1)  // error

  val x2: Bar^ = new Bar {}
  sink2(x2)
  sink1(x2)  // error
  sink2(x2)  // error

  val x3: Bar^ = new Bar {}
  sink3(x3)
  sink3(x3)  // ok, rd/rd
  sink2(x3)  // error

  val x4: Bar^ = new Bar {}
  sink2(x4)
  sink3(x4)  // error
  sink2(x4)  // error




