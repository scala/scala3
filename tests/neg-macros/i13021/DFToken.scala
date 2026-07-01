trait Token:
  val data: Any

opaque type DFToken = Token
object DFToken:
  extension (of: DFToken) def asIR: Token = ???

  opaque type Of[D] <: DFToken = DFToken
  object Of:
    extension [D](token: Of[D]) def width(using w: Width[?]): Int = ???

def getWidth[W <: Int](token: DFBits.Token[W]): Int = token.width
def getData[W <: Int](token: DFBits.Token[W]): Int =
  token.data // error
