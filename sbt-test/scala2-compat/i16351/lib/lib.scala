// Should be compiled with 2.13
package lib

class Value(val value: String)

class Lib(
  value: => Value,
  a: Int = 0,
  b: Int
)
