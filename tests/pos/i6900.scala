object Test {
  given bla[A]: { def [C](a: A) foo: C => A = _ => a }

  1.foo.foo
  1.foo.foo[String]
  1.foo[String].foo
  1.foo[String].foo[String]
}
