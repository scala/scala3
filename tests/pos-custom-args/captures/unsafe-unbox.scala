import annotation.unchecked.uncheckedCaptures
def test =
  val finalizeActionsInit: (collection.mutable.ListBuffer[(() => Unit) @uncheckedCaptures]^) @uncheckedCaptures
    = collection.mutable.ListBuffer()
  var finalizeActions = finalizeActionsInit
  val action = finalizeActions.remove(0)


