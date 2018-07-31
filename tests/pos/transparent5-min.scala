object Test {
  dependent def foo(x: Option[Int]): Option[Int] =
    foo(x)
}
