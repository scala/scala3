def foo(implicit f: () => Unit): Unit = ???

@main def main =
  // `using` can automatically be added when the application is done with parentheses
  foo ( () => 43 ) // warn
  // `using` cannot automatically be added when the application is done with trailing lambda syntax
  foo { () => 43 } // warn
