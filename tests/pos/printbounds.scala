class Tree[-T >: Number]


class Test {

  val x: Tree[?] = ???

  val y = x // With -Xprint:typer this should print  val x: Tree[_] = x
            // used to print Tree[Nothing], which is confusing.

}
