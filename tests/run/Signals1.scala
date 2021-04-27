
import annotation.unchecked.*
package frp:

  trait Signal[+T]:
    def apply()(using caller: Signal.Caller): T

  object Signal:

    abstract class AbstractSignal[+T] extends Signal[T]:
      private var currentValue: T = compiletime.uninitialized
      private var observers: Set[Caller] = Set()

      protected def eval: Caller => T

      protected def computeValue(): Unit =
        val newValue = eval(this)
        val observeChange = observers.nonEmpty && newValue != currentValue
        currentValue = newValue
        if observeChange then
          val obs = observers
          observers = Set()
          obs.foreach(_.computeValue())

      def apply()(using caller: Caller): T =
        observers += caller
        assert(!caller.observers.contains(this), "cyclic signal definition")
        currentValue
    end AbstractSignal

    def apply[T](expr: Caller ?=> T): Signal[T] =
      new AbstractSignal[T]:
        protected val eval = expr(using _)
        computeValue()

    class Var[T](expr: Caller ?=> T) extends AbstractSignal[T]:
      protected var eval: Caller => T = expr(using _)
      computeValue()

      def update(expr: Caller ?=> T): Unit =
        eval = expr(using _)
        computeValue()
    end Var

    opaque type Caller = AbstractSignal[?]
    given noCaller: Caller = new AbstractSignal[Nothing]:
      override def eval = ???
      override def computeValue() = ()

  end Signal
end frp

import frp.*
class BankAccount:
  def balance: Signal[Int] = myBalance

  private val myBalance: Signal.Var[Int] = Signal.Var(0)

  def deposit(amount: Int): Unit =
    if amount > 0 then
      val b = myBalance()
      myBalance() = b + amount

  def withdraw(amount: Int): Int =
    if 0 < amount && amount <= balance() then
      val b = myBalance()
      myBalance() = b - amount
      myBalance()
    else throw new AssertionError("insufficient funds")
end BankAccount

@main def Test() =
  def consolidated(accts: List[BankAccount]): Signal[Int] =
    Signal(accts.map(_.balance()).sum)

  val a = BankAccount()
  val b = BankAccount()
  val c = consolidated(List(a, b))
  println(c())
  a.deposit(10)
  println(c())
  b.deposit(20)
  println(c())
end Test
