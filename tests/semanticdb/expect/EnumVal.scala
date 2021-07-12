package enumVal

import scala.runtime.EnumValue


trait A

enum Color(val rgb: Int):
  case Red   extends Color(0xFF0000) with EnumValue
  case Green extends Color(0x00FF00) with A
  case Blue  extends Color(0x0000FF)
