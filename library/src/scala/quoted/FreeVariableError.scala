package scala.quoted

class FreeVariableError(val name: String) extends QuoteError("Free variable " + name + " could not be handled")
