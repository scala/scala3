package transactional

case class Reader[R,A](run: R => A) {
  def map[B](f: A => B): Reader[R, B] = Reader(r => f(run(r)))
  def flatMap[B](f: A => Reader[R, B]): Reader[R, B] = Reader(r => f(run(r)).run(r))
}

object Reader {
  def ask[R]: Reader[R,R] = Reader(r => r)
}

object ReaderBench extends Benchmark {
  type Transactional[T] = Reader[Transaction, T]

  def transaction[T](op: Transactional[T]): T = {
    implicit val trans: Transaction = new Transaction
    val res = op.run(trans)
    trans.commit()
    res
  }

  def thisTransaction: Transactional[Transaction] = Reader.ask

  abstract class Op {
    def f(x: Int): Transactional[Int]
  }

  class Op0 extends Op {
    def f(x: Int): Transactional[Int] =
      for (trans <- thisTransaction)
      yield { trans.println("0th step"); x }
  }

  class Op1 extends Op {
    def f(x: Int): Transactional[Int] =
      for (trans <- thisTransaction)
      yield { trans.println("first step"); x + 1 }
  }

  class Op2 extends Op {
    def f(x: Int): Transactional[Int] =
      for (trans <- thisTransaction)
      yield { trans.println("second step"); x + 2 }
  }

  class Op3 extends Op {
    def f(x: Int): Transactional[Int] =
      for (trans <- thisTransaction)
      yield { trans.println("third step"); x + 3 }
  }

  val op = Array[Op](new Op0, new Op1, new Op2, new Op3)

  def f(x: Int, n: Int): Transactional[Int] = {
    def rest(trans: Transaction): Transactional[Int] = {
      trans.println("fourth step")
      if (n > 0) {
        for {
          y <- op(n % 4).f(x)
          z <- f(y: Int, n - 1)
        }
        yield z
      }
      else {
        if (x % 2 != 0)
          for (trans <- thisTransaction)
          yield { trans.abort(); () }
        Reader(_ => x)
      }
    }
    thisTransaction.flatMap(rest)
  }

  def run(): Int = {
    transaction {
      for (res <- f(7, 10))
      yield {
        for (trans <- thisTransaction)
        yield { assert(!trans.isAborted); () }
        assert(res == 22)
        res
      }
    }
  }
}

object ReaderMonadic extends Runner("reader monadic", ReaderBench, 22)
