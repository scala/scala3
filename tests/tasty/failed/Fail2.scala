class Test() {
  thisTest => //pickling representation is the same if remove self renaming

  import Test._

  val myStatus: Status = Unknown

  def currentStatus: Status = myStatus
}

object Test {
  private type Status = Byte
  val Unknown: Status = 0
}
