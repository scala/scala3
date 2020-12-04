object Test2 {

  // Works with extension method
  extension [A](a: A)
    def foo[C]: C => A = _ => a   // error: extension method cannot have type parameters

  1.foo.foo

  // ... but have to pass 2 parameters
  1.foo.foo[Any => Int, String]
  1.foo[Int, String].foo
  1.foo[Int, String].foo[String => Int, String]

}

