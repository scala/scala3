
@main def Test: Unit =
  testValueOfType[true]
  testValueOfType[false]
  // TODO support Byte and short literal types
  // testValueOfType[1b]
  // testValueOfType[2s]
  testValueOfType[3]
  testValueOfType[4]
  testValueOfType[5L]
  testValueOfType[6d]
  testValueOfType[7f]
  testValueOfType['a']
  testValueOfType["abc"]
  val x: 10 = 10
  testValueOfType[x.type]
  type A = 11
  testValueOfType[A]


  testValueOfType[Boolean]
  testValueOfType[Byte]
  testValueOfType[Short]
  testValueOfType[Int]
  testValueOfType[Long]
  testValueOfType[Double]
  testValueOfType[Float]
  testValueOfType[Char]
  testValueOfType[String]
  testValueOfType[Null]
  testValueOfType[Any]
  testValueOfType[Some[1]]
