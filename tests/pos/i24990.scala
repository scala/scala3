//> using options -language:experimental.erasedDefinitions

object test {
  opaque type Foo = Int

  class Bar[A] extends compiletime.Erased
  object Bar { inline given inst[A]: Bar[A] = new Bar[A] }
}

def takesBar[A](a: A)(using b: test.Bar[A]): A = a

val _ = takesBar(1)
