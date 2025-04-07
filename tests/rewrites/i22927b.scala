
def foo(implicit f: () => Unit): Unit = ???
def bar(a: Int)(implicit f: () => Unit): Unit = ???

@main def main =
  // Trailing lambdas with braces:
  foo { () => 43 }
  foo { () => val x = 42; 43 }
  foo{ () => val x = 42; 43 }
  foo {() => val x = 42; 43}
  bar(1) { () =>
    val x = 42
    43 }

  // Parentheses:
  foo ( () => 43 )
  foo ( () =>
    val x = 42
    43
  )
  foo( () =>
    val x = 42
    43
  )
  foo (() =>
    val x = 42
    43
  )
  bar(1) ( () =>
    val x = 42
    43 )

  // Trailing lambdas with column and indentation:
  foo: () =>
    43
  foo : () =>
    val x = 42
    43
  foo :() =>
    val x = 42
    43
  foo
  : () =>
    val x = 42
    43
  foo
  :
    () =>
      val x = 42
      43
  bar(1) : () =>
    val x = 42
    43

