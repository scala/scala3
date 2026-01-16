trait Cap:
  def use(): Unit

def withCap[T](op: (x: Cap^) => T): T = ???

trait Box:
  val get: () ->{} () => Cap^

def main(): Unit =
  val leaked = withCap: (io: Cap^) =>
    class Fuzz extends Box, caps.Pure: // error
      self =>
      val get: () ->{} () ->{io} Cap^ =
        () => () => io
    class Foo extends Box, caps.Pure: // error
      val get: () ->{} () ->{io} Cap^ =
        () => () => io
    new Foo
  val bad = leaked.get()().use()  // using a leaked capability

