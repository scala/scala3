//> using options -language:experimental.erasedDefinitions

object test {
  opaque type Foo = Int

  class Bar[A] extends compiletime.Erased
  object Bar { inline given inst[A]: Bar[A] = new Bar[A] }
}

def takesBar[A](a: A)(using b: test.Bar[A]): A = a

// TODO: this should be pure, see #26800 #24990 #26578
val _ = takesBar(1) // error
