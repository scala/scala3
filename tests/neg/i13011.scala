class i13011 {
  lazy implicit val simple1: String = simple1 // error
  def f: Unit = {
    lazy val simple2: String = simple2 // error
  }

  lazy val simple3: String = if true then this.simple3 else "a" // error

  lazy val simple4: String = identity(this.simple4) // error
        
  lazy val simple5: String = {  // error
    this.simple5
    "aa"
  }

  lazy val simple6: Function0[Any] = () => this.simple6  // Ok
}
