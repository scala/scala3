//> using options -Wunused:privates

trait A:
  def get: Any = new A.C

object A:
  private class C[T]

trait B:
  def get: Any = new B.C

object B:
  private class C
