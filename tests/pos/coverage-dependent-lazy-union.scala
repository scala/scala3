def unionTest: Unit =
  lazy val x = new Object
  def accept(y: x.type | String): Unit = ()
  accept(x)
