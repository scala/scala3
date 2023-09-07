transparent inline def foo(inline f: [X] => X => X): Int = f[Int](1)

@main def Test: Unit =
  val code = compiletime.codeOf(foo([X] => (x: X) => { println(x); x }))
  println(code)