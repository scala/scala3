// scalac: -Ycheck-all-patmat
class C {
  trait Foo
  class One extends Foo
  class Two extends Foo
  class Bla extends One
}

class Test(val c: C) {
  import c.*
  def test(f: Foo) = f match { // not exhaustive
    case f: One =>
    case f: Two =>
    case f: Bla => // unreachable case
  }
}
