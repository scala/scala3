object Test2 {

  // Works with extension method
  extension on [A](a: A):
    def foo[C]: C => A = _ => a   // error: extension method cannot have type parameters

  1.foo.foo                      // error: foo is undefined

  // ... but have to pass 2 parameters
  1.foo.foo[Any => Int, String]                  // error: foo is undefined
  1.foo[Int, String].foo                         // error: foo is undefined
  1.foo[Int, String].foo[String => Int, String]  // error: foo is undefined

}

