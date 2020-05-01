trait InlineConstants {
  inline val myInlinedBoolean: Boolean
  inline val myInlinedByte: Byte
  inline val myInlinedShort: Short
  inline val myInlinedInt: Int
  inline val myInlinedLong: Long
  inline val myInlinedFloat: Float
  inline val myInlinedDouble: Double
  inline val myInlinedChar: Char
  inline val myInlinedString: String
}

object Constants extends InlineConstants {
  inline val myInlinedBoolean = true
  inline val myInlinedByte = 1
  inline val myInlinedShort = 2
  inline val myInlinedInt = 3
  inline val myInlinedLong = 4
  inline val myInlinedFloat = 5
  inline val myInlinedDouble = 6
  inline val myInlinedChar = 'a'
  inline val myInlinedString = "abc"
}

class Constants2 {
  inline val myInlinedBoolean: Boolean = true
  inline val myInlinedByte: Byte = 1
  inline val myInlinedShort: Short = 2
  inline val myInlinedInt: Int = 3
  inline val myInlinedLong: Long = 4
  inline val myInlinedFloat: Float = 5
  inline val myInlinedDouble: Double = 6
  inline val myInlinedChar: Char = 'a'
  inline val myInlinedString: String = "abc"
}