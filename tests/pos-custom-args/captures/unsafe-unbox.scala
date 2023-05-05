import annotation.unchecked.uncheckedCaptures
def test =
  @uncheckedCaptures
  var finalizeActions = collection.mutable.ListBuffer[() => Unit]()
  val action = finalizeActions.remove(0)


