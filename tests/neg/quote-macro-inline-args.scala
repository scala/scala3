object Test {

  inline def foo(inline x: Int): Int = ~{
    if (x == 1) '(3)
    else '(x) // error
  }

}

