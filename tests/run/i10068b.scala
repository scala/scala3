trait Foo[A] {
  def xxx(a1: A, a2: A): A
  def xxx(a: A): A = xxx(a, a)
}

trait Bar[A] extends Foo[A] {
  def yyy(a1: A, a2: A) = xxx(a1, a2)
}

trait Baz[A] extends Bar[A]

object Test:
  def main(args: Array[String]): Unit =
    val foo: Foo[String] = { (s1, s2) => s1 ++ s2 }
    val bar: Bar[String] = { (s1, s2) => s1 ++ s2 }
    val baz: Baz[String] = { (s1, s2) => s1 ++ s2 }

    val s = "abc"
    val ss = "abcabc"
    assert(foo.xxx(s) == ss)
    assert(bar.yyy(s, s) == ss)
    assert(baz.xxx(s) == ss)
    assert(baz.yyy(s, s) == ss)
