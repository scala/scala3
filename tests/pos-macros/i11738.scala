import scala.quoted.*

def blah[A](using Quotes, Type[A]): Expr[Unit] =
  Type.of[A] match
    case '[h *: t] => println(s"h = ${Type.show[h]}, t = ${Type.show[t]}") // ok
    case '[type f[X]; f[a]]   => println(s"f = ${Type.show[f]}, a = ${Type.show[a]}") // error
    case _         =>
  '{()}
