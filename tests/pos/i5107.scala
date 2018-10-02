class Test {
  inline def foo(x: Int = 5)(implicit y: Int): Int =
    if (x > 0) y * y
    else y + y

  implicit val m: Int = 7

  (new Test).foo()
}
