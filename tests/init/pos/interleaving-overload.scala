//> using options -experimental

import scala.language.experimental.clauseInterleaving

class A{

    def f1[T](x: Any)[U] = ???
    def f1[T](x: Int)[U] = ???

    f1(1)
    f1("hello")
    f1[Boolean]("a")[Int]
    f1[Boolean](1)[Int]

    case class B[U](x: Int)
    def b[U](x: Int) = B[U](x)

    def f2[T]: [U] => Int => B[U] = [U] => (x: Int) => b[U](x)

    f2(1)
    f2[Any](1)
    f2[Any][Any](1)

    b[Int](5)

}