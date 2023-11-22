import annotation.unchecked.uncheckedCaptures
def test =
  val finalizeActionsInit = collection.mutable.ListBuffer[(() => Unit) @uncheckedCaptures]()
  var finalizeActions = finalizeActionsInit
  val action = finalizeActions.remove(0)


