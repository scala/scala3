//> using options -language:experimental.erasedDefinitions

type F = (x: Int, erased y: Int) => Int

class A extends compiletime.Erased

@main def Test() =
  val a: F = (x, y) => x + 1 // error: Expected F got (Int, Int) => Int
  val b: F = (x, erased y) => x + 1 // ok
  val c: F = (_, _) => 5 // error: Expected F got (Int, Int) => Int
  val d: F = (_, erased _) => 5 // ok

  def use(f: F) = f(5, 6)

  use { (x, y) => x } // error: Expected F got (Int, Int) => Int

  def singleParam(f: (erased x: Int) => Int) = f(5)

  singleParam(x => 5) // error: Expected (erased Int) => Int got Int => Int
  singleParam((erased x) => 5) // ok

  def erasedClass(f: A => Int) = f(new A)

  erasedClass(_ => 5) // ok since A is implicitly erased

