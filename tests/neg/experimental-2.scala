class Test7 {
  import scala.language.experimental
  import experimental.genericNumberLiterals // error: no aliases can be used to refer to a language import
  val x: BigInt = 13232202002020202020202   // error
}
