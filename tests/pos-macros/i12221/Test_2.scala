@main
def hello: Unit = {
    abstract class A
    case class Foo(a: Int) extends A

    //Will print the body of the given function, then its print (using Printer.TreeStructure)
    Macros.show((x: A)=>{
        x match {
            case Foo(a) => a
        }:Int
    })
    /*
    val x = Foo(3)
    Macros.show(
        x match {
            case Foo(1) => 3
        }
    )*/

}
