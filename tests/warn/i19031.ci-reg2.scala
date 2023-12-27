trait Outer:
  sealed trait Foo
  case class  Bar1() extends Foo
  case class  Bar2() extends Foo
  case object Bar3   extends Foo

  def foos: List[Foo]

class Test:
  def t1(out: Outer) = out.foos.collect:
    case out.Bar1() => 1
    case out.Bar2() => 2
    case out.Bar3   => 3
