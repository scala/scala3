abstract class C1[A1]:
  def set(x: A1): Unit
  def get: A1

trait Co[+A]:
  def get: A

class C2[sealed A2] extends C1[A2], Co[A2]:  // ok
  private var x: A2 = ???
  def set(x: A2): Unit =
    this.x = x
  def get: A2 = x

class C3[A3] extends C2[A3] // error

abstract class C4[sealed A4] extends Co[A4] // ok

abstract class C5[sealed +A5] extends Co[A5] // ok

abstract class C6[A6] extends C5[A6] // error

