object Test {
  inline def foo(inline n: Int) = bar(n)
  inline def bar(inline n: Int) = n
}
