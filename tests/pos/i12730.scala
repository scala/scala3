class ComponentSimple

class Props {
  def apply(props: Any): Any = ???
}

class Foo[C] {
  def build: ComponentSimple = ???
}

class Bar[E] {
  def render(r: E => Any): Unit = {}
}

trait Conv[A, B] {
  def apply(a: A): B
}

object Test {
  def toComponentCtor[F](c: ComponentSimple): Props = ???

  def defaultToNoBackend[G, H](ev: G => Foo[H]): Conv[Foo[H], Bar[H]] = ???

  def conforms[A]: A => A = ???

  def problem = Main // crashes

  def foo[H]: Foo[H] = ???

  val NameChanger =
    foo
      .build

  val Main =
    defaultToNoBackend(conforms).apply(foo)
      .render(_ => toComponentCtor(NameChanger)(13))
}
