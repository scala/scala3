object S {
  val BooleanTrue: true = J.BooleanTrue
  val BooleanFalse: false = J.BooleanFalse
  val InvertedBoolean: false = J.InvertedBoolean

  val PositiveByte: 23 = J.PositiveByte
  val NegativeByte: -42 = J.NegativeByte
  val LiteralCharAsByte: 'a' = J.LiteralCharAsByte

  val NumChar: 33 = J.NumChar
  val LiteralChar: 'b' = J.LiteralChar

  val PositiveShort: 0x1234 = J.PositiveShort
  val NegativeShort: -0x5678 = J.NegativeShort
  val LiteralCharAsShort: 'c' = J.LiteralCharAsShort

  val PositiveInt: 0xabcdef = J.PositiveInt
  val NegativeInt: -12345678 = J.NegativeInt
  val LiteralCharAsInt: 'd' = J.LiteralCharAsInt

  val PositiveLong: 0x1234567890abcdefL = J.PositiveLong
  val NegativeLong: -0xfedcba09876L = J.NegativeLong
  val LiteralCharAsLong: 'e' = J.LiteralCharAsLong
  val LiteralIntAsLong: 0x12345678 = J.LiteralIntAsLong

  val PositiveFloat: 42.232323f = J.PositiveFloat
  val NegativeFloat: -3.1415f = J.NegativeFloat

  val PositiveDouble: 23.4243598374594d = J.PositiveDouble
  val NegativeDouble: -42.2324358934589734859d = J.NegativeDouble

  val RegularString: "testConstant" = J.RegularString
  val NegativeString: "!#!$!grml%!%!$#@@@" = J.NegativeString
}