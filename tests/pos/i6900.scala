object Test {
  given bla[A] { def (a: A) foo[C]: C => A = _ => a }

  1.foo.foo
  1.foo.foo[String]
  1.foo[String].foo
  1.foo[String].foo[String]
}
