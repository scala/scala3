import caps.unsafeUnbox
type ErrorHandler = (Int, String) => Unit

var defaultIncompleteHandler: ErrorHandler = ???
var incompleteHandler: ErrorHandler = defaultIncompleteHandler
//val x = incompleteHandler.unsafeUnbox


