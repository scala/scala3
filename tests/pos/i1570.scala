object Test {
  inline def foo(transparent n: Int) = bar(n)
  inline def bar(transparent n: Int) = n
}
