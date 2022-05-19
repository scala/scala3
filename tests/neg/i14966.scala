trait I[A]:
  def f(x: List[String]): A

trait S:
  def f(x: List[Int]): Int

trait T[A] extends I[A], S

class Test(s: T[Int]):
  export s.*  // error

