import language.experimental.captureChecking

class EscapeIO
class EscapeRepro(val io: EscapeIO^)
final class EscapeBox[T](val value: T)

def escapeFactory[T](clazz: Class[T], value: T): EscapeBox[T] = new EscapeBox(value)

def rejectEscape(io: EscapeIO^): Unit =
  identity[EscapeBox[EscapeRepro^{}]](escapeFactory(classOf[EscapeRepro], new EscapeRepro(io))) // error
