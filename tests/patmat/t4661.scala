trait Foo
class One extends Foo
class Two extends Foo
class Three extends Foo

object Test {
  def test(f: Foo) = f match {
    case f: Foo =>
    case f: One =>   // unreachable
    case f: Two =>   // unreachable
    case f: Three => // unreachable
  }
}

trait Prefix {
  sealed trait Bar
  class BarOne extends Bar
  class BarTwo extends Bar
  class BarThree extends Bar
}

class TestPrefix(val p: Prefix) {
  import p.*
  def test(b: Bar) = b match {
    case b: Bar =>
    case b: BarOne =>   // unreachable
    case b: BarTwo =>   // unreachable
    case b: BarThree => // unreachable
  }

  def test2(b: Bar) = b match {
    case b: Prefix#BarOne =>
    case b: BarOne =>  // unreachable
    case b: Prefix#BarTwo =>
    case b: Prefix#BarThree =>
    case b: BarTwo =>     // unreachable
    case b: BarThree =>   // unreachable
    case b: Bar =>        // unreachable
    case b: Prefix#Bar => // unreachable
    case _ => // only null matches
  }

  def test3(b: Prefix#Bar) = b match {
    case b: Bar =>
    case b: Prefix#BarOne =>
    case b: BarOne => // unreachable
    case _ =>
  }

  def test4(b: Bar) = b match {
    case b: Bar =>
    case b:Prefix#Bar => // unreachable
  }

  def test5(b: Bar) = b match {
    case b: Prefix#BarOne =>
    case b: Prefix#BarTwo =>
    case b: Prefix#BarThree =>
    case _ => // only null matches
  }
}
