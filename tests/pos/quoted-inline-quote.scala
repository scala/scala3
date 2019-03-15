class Foo {
  inline def foo(x: quoted.Expr[String]) = '{ println(${x}) }
  foo('{"abc"})
}