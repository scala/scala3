
import annotation.unchecked.*
import compiletime.uninitialized
package frp:

  sealed class Signal[+T](expr: Signal.Caller ?=> T):
    private var myExpr: Signal.Caller => T = uninitialized
    private var myValue: T = uninitialized
    private var observers: Set[Signal.Caller] = Set()
    changeTo(expr)

    protected def changeTo(expr: Signal.Caller ?=> T @uncheckedVariance): Unit =
      myExpr = (caller => expr(using caller))
      computeValue()

    def apply()(using caller: Signal.Caller) =
      observers += caller
      assert(!caller.observers.contains(this), "cyclic signal definition")
      myValue

    protected def computeValue(): Unit =
      val newValue = myExpr(this)
      val observeChange = observers.nonEmpty && newValue != myValue
      myValue = newValue
      if observeChange then
        val obs = observers
        observers = Set()
        obs.foreach(_.computeValue())

  object Signal:
    type Caller = Signal[?]
    given noCaller: Caller(???):
      override def computeValue() = ()
  end Signal

  class Var[T](expr: Signal.Caller ?=> T) extends Signal[T](expr):
    def update(expr: Signal.Caller ?=> T): Unit = changeTo(expr)
  end Var
end frp

import frp.*
class BankAccount:
  def balance: Signal[Int] = myBalance

  private var myBalance: Var[Int] = Var(0)

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
