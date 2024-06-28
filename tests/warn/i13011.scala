

class i13011 {
  lazy implicit val simple1: String = simple1 // warn
  def f: Unit = {
    lazy val simple2: String = simple2 // warn
  }

  lazy val simple3: String = if true then this.simple3 else "a" // warn

  def firstDigitIsEven(n: Int): Boolean = if n % 10 == n then n % 2 == 0 else firstDigitIsEven(n / 10)

  lazy val simple4: String = if firstDigitIsEven(22) then this.simple4 else "a" // ok

  lazy val simple5: String = identity(this.simple5) // warn

  lazy val simple6: String = {  // warn
    this.simple6
    "aa"
  }

  lazy val simple7: Function0[Any] = () => this.simple7  // Ok
}