// This test is based on sconfig/sconfig/shared/src/main/scala/org/ekrich/config/impl/SimpleConfig.scala
// In cases where a nullable variable is assigned a flexible type, we want
// later code to type the variable access as non-nullable
// Added in https://github.com/scala/scala3/pull/24278

def main() =
  var result: String | Null = null
  result = "".trim()
  result.trim()