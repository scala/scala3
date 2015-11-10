class A{
  private def foo = 1
  def callsFoo1(other: A & B): Int = other.foo
  def callsFoo2(other: B & A): Int = other.foo
}

trait B {
  def foo(i: Int) = i
}

object Test {
  def main(args: Array[String]): Unit = {
    val a = new A with B
    a.callsFoo1(a)
    a.callsFoo2(a)	
  }
}
