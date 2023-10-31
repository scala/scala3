import scala.quoted.*
def impl[E: Type](ref: Expr[Foo[?]])(using Quotes): Expr[Unit] = '{ }
