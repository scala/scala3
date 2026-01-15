trait Cap extends caps.SharedCapability:
  def use(): Unit

def withCap[T](op: (x: Cap) => T): T = ???

trait Box:
  val get: () ->{} () => Cap

def main(): Unit =
  val leaked = withCap: (io: Cap) =>
    class Fuzz extends Box, caps.Pure:
      self =>
      val get: () ->{} () ->{io} Cap =
        () => () => io // error
    class Foo extends Box, caps.Pure:
      val get: () ->{} () ->{io} Cap =
        () => () => io // error
    new Foo
  val bad = leaked.get()().use()  // using a leaked capability

