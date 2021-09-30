trait Foo[F[_]]:
  extension [A](fa: F[A])
    def foo[B](fb: F[B]): Int

def test1 =
  // Simplified from https://github.com/typelevel/spotted-leopards/issues/2
  given listFoo: Foo[List] with
    extension [A](fa: List[A])
      def foo[B](fb: List[B]): Int = 1

  given functionFoo[T]: Foo[[A] =>> T => A] with
    extension [A](fa: T => A)
      def foo[B](fb: T => B): Int = 2

  val x = List(1, 2).foo(List(3, 4))
  assert(x == 1, x)

def test2 =
  // This test case would fail if we used `wildApprox` on the method types
  // instead of using the correct typer state.
  trait Bar1[A]:
    extension (x: A => A) def bar(y: A): Int
  trait Bar2:
    extension (x: Int => 1) def bar(y: Int): Int

  given bla1[T]: Bar1[T] with
    extension (x: T => T) def bar(y: T): Int = 1
  given bla2: Bar2 with
    extension (x: Int => 1) def bar(y: Int): Int = 2

  val f: Int => 1 = x => 1
  val x = f.bar(1)
  assert(x == 2, x)

@main def Test =
  test1
  test2
