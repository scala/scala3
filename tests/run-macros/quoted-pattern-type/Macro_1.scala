import scala.quoted._

object Lib {

  inline def foo[T](inline arg: T): T = ${ impl('arg) }

  private def impl[T: Type](arg: Expr[T])(using Quotes): Expr[T] = {
    arg match {
      case '{ $x: Boolean } as e => '{ println("Boolean: " + $e); $e }
      case '{ $x: Int } as e => '{ println("Int: " + $x); $x }
      case '{ println("hello"); $arg } => '{ println("Printed hello and returned " + $arg); $arg }
      case '{ println("world"); $arg: T } => '{ println("Printed world and returned " + $arg); $arg }
      case '{ Some($x: Int) } as e => '{ println("Some: " + $x); $e }
      case arg => '{ ??? }
    }
  }
}
