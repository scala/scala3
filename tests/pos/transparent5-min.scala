object Test {
  transparent def foo(x: Option[Int]): Option[Int] =
    foo(x)
}
