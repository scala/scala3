object Test {
  object Foo
}

class Test {
  class Foo  // error: name clash
}

object Test2 {
  class Foo
}

class Test2 {
  object Foo // error: name clash
}
