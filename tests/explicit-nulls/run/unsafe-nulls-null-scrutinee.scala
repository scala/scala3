// Under unsafe nulls, `null` is still a value of every reference type, so a scrutinee of
// type `Null` must not be treated as known to be non-null. Otherwise a type test against a
// reference type is folded to a constant `true`, and the pattern matcher omits the null test
// that guards the placeholder plan it emits for a scrutinee of bottom type.

object Test {
  import scala.language.unsafeNulls

  def typeTest: Int = null match {
    case _: AnyRef => 1
    case _ => -1
  }

  def extractor: String =
    try {
      val Some(_) = null: @unchecked
      "matched"
    } catch {
      case _: MatchError => "MatchError"
    }

  def main(args: Array[String]): Unit = {
    // evaluated before asserting, so that both are exercised on every run
    val t = typeTest
    val e = extractor
    assert(t == -1, s"type test on a null scrutinee gave $t")
    assert(e == "MatchError", s"extractor on a null scrutinee gave $e")
  }
}
