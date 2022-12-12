import caps.unsafe.*
def test =
  var finalizeActions = collection.mutable.ListBuffer[() => Unit]()
  val action = finalizeActions.remove(0).unsafeUnbox
