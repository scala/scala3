object Foo {
  val program = '{
    val tpe: quoted.Type[Int] = ???
    val expr: quoted.Expr[Int] = ???

    val a: quoted.Expr[Int] = ${ '[Int] } // error
    val b: quoted.Expr[Int] = '{ $tpe } // error
    val c: ${ '{ 43 } } = ???  // error
    val d: quoted.Type[Int] = '[ $expr ] // error
  }
}
