object Test {
     def foo(a1: String*) = a1
//     val fooEta = foo _
     foo: (Seq[String] => Seq[String])
}
