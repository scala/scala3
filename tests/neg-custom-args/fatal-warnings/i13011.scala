class i13011 {
  lazy implicit val simple1: String = simple1 // error
  def f: Unit = {
    lazy val simple2: String = simple2 // error
  }

  lazy val simple3: String = if true then this.simple3 else "a" // error

  def firstDigitIsEven(n: Int): Boolean = if n % 10 == n then n % 2 == 0 else firstDigitIsEven(n / 10)

  lazy val simple4: String = if firstDigitIsEven(22) then this.simple4 else "a" // error

  lazy val simple5: String = identity(this.simple5) // error
        
  lazy val simple6: String = {  // error
    this.simple6
    "aa"
  }

  lazy val simple7: Function0[Any] = () => this.simple7  // Ok
}
