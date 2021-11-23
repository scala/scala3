
class A{
    /*
    def f0[T](x: Any) = ???
    def f0[T](x: Int) = ???
    */

    def f1[T][U](x: Any) = ???
    def f1[T][U](x: Int) = ???

    //f0(1)
    f1(1)
    f1("hello")

    case class B[U](x: Int)
    def b[U](x: Int) = B[U](x)

    def f2[T]: [U] => Int => B[U] = [U] => (x: Int) => b[U](x)

    
    f2(1)
    f2[Any](1)
    f2[Any][Any](1)

}