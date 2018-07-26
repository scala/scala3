object Test {
  transparent def foo(transparent n: Int) = bar(n)
  transparent def bar(transparent n: Int) = n
}
