class Macro {
  inline def foo(x: quoted.Expr[String]) = '{ println(${x}) }
}