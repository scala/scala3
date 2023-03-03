import caps.unsafe.*

object Test:
  type ErrorHandler = (Int, String) => Unit

  var defaultIncompleteHandler: ErrorHandler = ???
  var incompleteHandler: ErrorHandler = defaultIncompleteHandler
  val x = incompleteHandler.unsafeUnbox
  val _ : ErrorHandler = x
  val _ = x(1, "a")

  def defaultIncompleteHandler1(): ErrorHandler = ???
  val defaultIncompleteHandler2: ErrorHandler = ???
  var incompleteHandler1: ErrorHandler = defaultIncompleteHandler1().unsafeBox
  var incompleteHandler2: ErrorHandler = defaultIncompleteHandler2.unsafeBox
  private var incompleteHandler7 = defaultIncompleteHandler1().unsafeBox
  private var incompleteHandler8 = defaultIncompleteHandler2.unsafeBox

  incompleteHandler1 = defaultIncompleteHandler2.unsafeBox
  incompleteHandler1 = defaultIncompleteHandler2.unsafeBox
  val saved = incompleteHandler1.unsafeUnbox
