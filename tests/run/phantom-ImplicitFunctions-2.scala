
import collection.mutable.ListBuffer

class Transaction {
  private val log = new ListBuffer[String]
  def println(s: String): Unit = log += s

  private var aborted = false
  private var committed = false

  def abort(): Unit = { aborted = true }
  def isAborted = aborted

  def commit(): Unit =
    if (!aborted && !committed) {
      Console.println("******* log ********")
      log.foreach(Console.println)
      committed = true
    }
}

object Test extends Phantom {
  type CanDoTransaction <: this.Any

  def transaction[T](op: Transaction => CanDoTransaction => T) = {
    val trans: Transaction = new Transaction
    op(trans)(assume)
    trans.commit()
  }

  def f1(x: Int)(implicit thisTransaction: Transaction, canDo: CanDoTransaction): Int = {
    thisTransaction.println(s"first step: $x")
    f2(x + 1)
  }
  def f2(x: Int)(implicit thisTransaction: Transaction, canDo: CanDoTransaction): Int = {
    thisTransaction.println(s"second step: $x")
    f3(x * x)
  }
  def f3(x: Int)(implicit thisTransaction: Transaction, canDo: CanDoTransaction): Int = {
    thisTransaction.println(s"third step: $x")
    if (x % 2 != 0) thisTransaction.abort()
    x
  }

  def main(args: Array[String]) = {
    transaction { implicit thisTransaction => implicit canDo =>
      val res = f1(args.length)
      println(if (thisTransaction.isAborted) "aborted" else s"result: $res")
    }
  }
}
