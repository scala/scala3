
import annotation.unchecked._
package frp with

  sealed class Signal[+T](expr: (given Signal.Caller) => T) with
    private var myExpr: Signal.Caller => T = _
    private var myValue: T = _
    private var observers: Set[Signal.Caller] = Set()
    changeTo(expr)

    protected def changeTo(expr: (given Signal.Caller) => T @uncheckedVariance): Unit =
      myExpr = (caller => expr(given caller))
      computeValue()

    def apply()(given caller: Signal.Caller) =
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

  object Signal with
    type Caller = Signal[?]
    given noCaller: Caller(???) with
      override def computeValue() = ()
  end Signal

  class Var[T](expr: (given Signal.Caller) => T) extends Signal[T](expr) with
    def update(expr: (given Signal.Caller) => T): Unit = changeTo(expr)
  end Var
end frp

import frp._
class BankAccount with
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
    else assertFail("insufficient funds")
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
