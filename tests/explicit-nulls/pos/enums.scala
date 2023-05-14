enum ErrorMessageID(val isActive: Boolean = true) extends java.lang.Enum[ErrorMessageID]:

  case NoExplanationID // errorNumber: -1
  case EmptyCatchOrFinallyBlockID extends ErrorMessageID(isActive = false) // errorNumber: 0

  def errorNumber = ordinal - 1

enum Color(val rgb: Int):
  case Red   extends Color(0xFF0000)
  case Green extends Color(0x00FF00)
  case Blue  extends Color(0x0000FF)

