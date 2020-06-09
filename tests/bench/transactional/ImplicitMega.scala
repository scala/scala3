package transactional
object MegaBench extends Benchmark {
  type Transactional[T] = Transaction ?=> T

  def transaction[T](op: Transactional[T]): T = {
    implicit val trans: Transaction = new Transaction
    val res = op
    trans.commit()
    res
  }

  def thisTransaction: Transactional[Transaction] = implicitly[Transaction]

  abstract class Op {
    def f(x: Int): Transactional[Int]
  }

  class Op0 extends Op {
    def f(x: Int): Transactional[Int] = {
      thisTransaction.println("0th step")
      x
    }
  }

  class Op1 extends Op {
    def f(x: Int): Transactional[Int] = {
      thisTransaction.println("first step")
      x + 1
    }
  }

  class Op2 extends Op {
    def f(x: Int): Transactional[Int] = {
      thisTransaction.println("second step")
      x + 2
    }
  }

  class Op3 extends Op {
    def f(x: Int): Transactional[Int] = {
      thisTransaction.println("third step")
      x + 3
    }
  }

  val op = Array[Op](new Op0, new Op1, new Op2, new Op3)

  def f(x: Int, n: Int): Transactional[Int] = {
    thisTransaction.println("fourth step")
    if (n > 0) f(op(n % 4).f(x), n - 1)
    else {
      if (x % 2 != 0) thisTransaction.abort()
      x
    }
  }

  def run(): Int = {
    transaction {
      val res = f(7, 10)
      assert(!thisTransaction.isAborted)
      assert(res == 22)
      res
    }
  }
}

object ImplicitMega extends Runner("megamorphic", MegaBench, 22)
