object Test {
  rewrite def foo(transparent n: Int) = bar(n)
  rewrite def bar(transparent n: Int) = n
}
