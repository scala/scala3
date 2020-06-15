package transactional
object MonoBench extends Benchmark {
  type Transactional[T] = Transaction ?=> T

  def transaction[T](op: Transactional[T]): T = {
    implicit val trans: Transaction = new Transaction
    val res = op
    trans.commit()
    res
  }

  def thisTransaction: Transactional[Transaction] = implicitly[Transaction]

  def f1(x: Int): Transactional[Int] = {
    thisTransaction.println("first step")
    f2(x + 1)
  }
  def f2(x: Int): Transactional[Int] = {
    thisTransaction.println("second step")
    f3(x * x)
  }
  def f3(x: Int): Transactional[Int] = {
    thisTransaction.println("third step")
    f4(x + 1, 7)
  }
  def f4(x: Int, n: Int): Transactional[Int] = {
    thisTransaction.println("fourth step")
    if (n > 0) f4(x + 1, n - 1)
    else {
      if (x % 2 != 0) thisTransaction.abort()
      x
    }
  }

  def run(): Int = {
    transaction {
      val res = f1(7)
      assert(!thisTransaction.isAborted)
      assert(res == 72)
      res
    }
  }
}

object ImplicitMono extends Runner("monomorphic implicits", MonoBench, 72)
