import caps.unsafe.*
import annotation.unchecked.uncheckedCaptures

object Test:
  type ErrorHandler = (Int, String) => Unit

  @uncheckedCaptures
  var defaultIncompleteHandler: ErrorHandler = ???
  @uncheckedCaptures
  var incompleteHandler: ErrorHandler = defaultIncompleteHandler
  private val x = incompleteHandler
  val _ : ErrorHandler = x
  val _ = x(1, "a")

  def defaultIncompleteHandler1(): (Int, String) => Unit = ???
  val defaultIncompleteHandler2: ErrorHandler = ???
  @uncheckedCaptures
  var incompleteHandler1: ErrorHandler = defaultIncompleteHandler1()
  @uncheckedCaptures
  var incompleteHandler2: ErrorHandler = defaultIncompleteHandler2
  @uncheckedCaptures
  private var incompleteHandler7 = defaultIncompleteHandler1()
  @uncheckedCaptures
  private var incompleteHandler8 = defaultIncompleteHandler2

  incompleteHandler1 = defaultIncompleteHandler2
  incompleteHandler1 = defaultIncompleteHandler2
  private val saved = incompleteHandler1


