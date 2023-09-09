trait Cap:
  def use(): Unit

def withCap[T](op: (x: Cap^) => T): T = ???

trait Box:
  val get: () ->{} () ->{cap} Cap^

def main(): Unit =
  val leaked = withCap: (io: Cap^) =>
    class Foo extends Box, Pure:
      val get: () ->{} () ->{io} Cap^ = // error
        () => () => io // error
    new Foo
  val bad = leaked.get()().use()  // using a leaked capability
