//> using options -language:experimental.erasedDefinitions

import scala.annotation.tailrec

class Foo1 extends compiletime.Erased
class Foo2

@tailrec
final def test1(n: Int, acc: Int): (Foo1, Foo2) ?=> Int =
  if n <= 0 then acc
  else test1(n - 1, acc * n)

@tailrec
final def test2(n: Int, acc: Int): Foo1 ?=> Int =
  if n <= 0 then acc
  else test2(n - 1, acc * n)

@main def Test() =
  given Foo1 = Foo1()
  given Foo2 = Foo2()
  test1(10, 0)
  test2(10, 0)
