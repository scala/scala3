object Foo {
  def foo(): Int = {
    val f: implicit Int => Int = (implicit x: Int) => 2 * x // error
    //     ^^^^^^^^
    //     an identifier expected, but 'implicit' found

    f given 2
  }

  val f = (implicit x: Int) => x // error // error
  //                 ^
  //                '=>' expected, but ':' found

  //                           ^
  //                           Not found: x


  ((implicit x: Int) => x): (implicit Int => Int) // error // error // error
  //          ^
  //             '=>' expected, but ':' found

  //                         ^^^^^^^^
  //                         an identifier expected, but 'implicit' found

  //                    ^
  //                    Not found: x

}
