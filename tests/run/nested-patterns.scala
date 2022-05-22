sealed abstract class T:
  def underlying: String
class A extends T { def underlying = "a" ; override def toString = "A" }
class B extends T { def underlying = "b" ; override def toString = "B" }
class C extends T { def underlying = "c" ; override def toString = "C" }

class Test:
  def test(x: Any, matchC: Boolean): Int = x match
    case i: Int => i
    case t: T if matchC && t.underlying.match
      case "c" => 3
    case t: T if t.underlying match
      case "a" => 1
      case "b" => 2
    case s: String => s.length
    case z => -1

object Test:
  def main(args: Array[String]): Unit =
    val t = new Test
    def run(x: Any, matchC: Boolean = false): Unit =
      println(s"$x -> ${t.test(x, matchC)}")

    run(0)

    run(new A)
    run(new B)
    run(new C)

    run(new A, matchC = true)
    run(new B, matchC = true)
    run(new C, matchC = true)

    run("str")
    run(())
