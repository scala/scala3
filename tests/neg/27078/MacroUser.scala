package sample

import sample.JavaAnnotation

class MacroUser:
  def someFunction(): Unit =
    val res = Macro.valueFromAnnotation[MacroUser.AnnotatedClass]
    println(res)

object MacroUser:
  @java.lang.Deprecated
  @Macro.AnnotWithValue(value = "some value")
  @JavaAnnotation(defaultedValue = "filled") // error
  final class AnnotatedClass
