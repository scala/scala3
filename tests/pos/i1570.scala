object Test {
  transparent def foo(n: Int & Constant) = bar(n)
  transparent def bar(n: Int & Constant) = n
}
