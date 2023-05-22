import scala.quoted.*
def impl[E: Type](ref: Expr[Foo[_]])(using Quotes): Expr[Unit] = '{ }
