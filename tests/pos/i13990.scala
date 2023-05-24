
object Test:

  inline val myInt = 1 << 6

  // toLong
  inline val char2Long: 99L = 'c'.toLong
  inline val int2Long: 0L = 0.toLong
  inline val long2Long: 0L = 0L.toLong
  inline val int2LongPropagated: 64L = myInt.toLong

  // toInt
  inline val char2Int: 99 = 'c'.toInt
  inline val int2Int: 0 = 0.toInt
  inline val long2Int: 0 = 0L.toInt
  inline val long2IntWrapped: -2147483648 = 2147483648L.toInt
  inline val int2IntPropagated: 64 = myInt.toInt

  // toChar
  inline val char2Char: 'c' = 'c'.toChar
  inline val int2Char: 'c' = 99.toChar
  inline val long2Char: 'c' = 99L.toChar
  inline val int2CharPropagated: '@' = myInt.toChar

  // chain everything
  inline val wow: 1.0 = 1.toChar.toInt.toLong.toFloat.toDouble
