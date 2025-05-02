class Tree[-T >: Number]


class Test {

  val x: Tree[_] = ???

  val y = x // With -Vprint:typer this should print  val x: Tree[_] = x
            // used to print Tree[Nothing], which is confusing.

}
