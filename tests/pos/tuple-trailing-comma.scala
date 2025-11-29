// Test trailing comma syntax for tuples (B variant)
// This allows (A,) to be a single-element type tuple and (a,) to be a single-element value tuple

object TupleTrailingComma:
  // Type tuples with trailing comma
  type T1 = (Int,)                    // single-element type tuple
  type T2 = (Int, String,)            // trailing comma with multiple elements
  type T3 = (Int, String)             // regular tuple (should still work)

  // Value tuples with trailing comma
  val v1: (Int,) = (1,)               // single-element value tuple
  val v2 = (1, 2,)                    // trailing comma with multiple elements
  val v3 = (1, 2)                     // regular tuple (should still work)

  // Pattern matching with trailing comma
  def test(x: Any): Unit = x match
    case (a,) => println(s"single: $a")
    case (a, b,) => println(s"pair: $a, $b")
    case (a, b) => println(s"pair no trailing: $a, $b")
    case _ => println("other")
